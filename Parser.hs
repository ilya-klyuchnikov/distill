module Parser where

import Term
import qualified Language.Haskell.Syntax as L
import qualified Language.Haskell.Parser as P
import Data.List (delete)
import Data.Foldable (find)
import Debug.Trace

parseFile f = do
    contents <- readFile f
    let
        m = P.parseModule contents
    case m of
        (P.ParseOk n) ->
            let 
                (imports, dataDecls, main, funcs) = parseHsModule n
                (funcNames, funcBodies) = unzip funcs
                fixedFuncs = map (fixFuncs funcNames) funcBodies
                funcs' = zip funcNames fixedFuncs
            in case find (\(f, e) -> f == "root") funcs' of
                Just (f, e) -> return $ Program imports dataDecls main e (delete (f, e) funcs')
                _ -> error "No root function found"
        (P.ParseFailed src err) -> error (show src ++ "\n" ++ err)
        
parseHsModule (L.HsModule _ _ _ imports decls) = case findMain decls of
  Nothing -> error "No main function found"
  Just main -> (imports, (filterDataDecls decls), main, parseHsDecls (delete main decls))

findMain decls = getMain $ filterDecls decls

getMain [] = Nothing
getMain (f@(L.HsPatBind _ (L.HsPVar name) e []):xs) = case parseHsName name of
  "main" -> Just f
  _ -> getMain xs
getMain (x:xs) = getMain xs

parseHsDecls decls = map parseHsDecl $ filterDecls decls

filterDataDecls = filter isDataDecl

isDataDecl (L.HsPatBind {}) = False
isDataDecl (L.HsFunBind {}) = False
isDataDecl (L.HsTypeSig {}) = False
isDataDecl _ = True

filterDecls = filter isFuncDecl

isFuncDecl (L.HsPatBind {}) = True
isFuncDecl (L.HsFunBind {}) = True
isFuncDecl _ = False

fixFuncs names e@(Var v)
 | v `elem` names = Func v
 | otherwise = e
fixFuncs names (Con s es) = Con s $ map (fixFuncs names) es
fixFuncs names (Lambda v e) = Lambda v $ fixFuncs names e
fixFuncs names (App e e') = App (fixFuncs names e) (fixFuncs names e')
fixFuncs names (Case e bs) = Case (fixFuncs names e) (map (\(Branch c es e) -> (Branch c es $ fixFuncs names e)) bs)
fixFuncs names (Typed e t) = Typed (fixFuncs names e) t
fixFuncs names e = e


parseHsDecl (L.HsPatBind _ (L.HsPVar name) e []) =
    let
        fName = parseHsName name
        fBody = parseHsRhs e
    in (fName, fBody)
parseHsDecl (L.HsFunBind [L.HsMatch _ name pats e []]) =
    let
        args = map (\(L.HsPVar v) -> parseHsName v) pats
        fName = parseHsName name
        fBody = parseHsRhs e
        fExpr = foldr (\v e -> Lambda v e) fBody args
    in (fName, fExpr)
parseHsDecl d = error $ show d
    
parseHsRhs (L.HsUnGuardedRhs e) = parseHsExp e
parseHsRhs (L.HsGuardedRhss es) =
    let
        parsedGuards = map parseHsGuardedRhs es
        parsedExpr = buildGuardCase parsedGuards
    in parsedExpr
    
parseHsGuardedRhs (L.HsGuardedRhs _ c e) = (parseHsExp c, parseHsExp e)

buildGuardCase ((c, e):[]) = Case c [Branch "True" [] e]
buildGuardCase ((c, e):es) = Case c [Branch "True" [] e, Branch "False" [] $ buildGuardCase es]

parseHsName (L.HsIdent s) = s
parseHsName (L.HsSymbol s) = s

parseHsExp (L.HsVar v)
 | parseHsQName v == "otherwise" = Con "True" []
 | otherwise = Var $ parseHsQName v
parseHsExp (L.HsCon c) = Con (parseHsQName c) []
parseHsExp (L.HsLit (L.HsInt i)) = nat2con i
parseHsExp (L.HsLit l) = Lit l
parseHsExp (L.HsInfixApp e q e')
 -- PROBLEM HERE e.g. input = (1:2:1) parsed as ((1:2):1), with no nil.
 | parseHsQOp q == "Cons" = Con "Cons" [parseHsExp e, parseHsExp e']
 | parseHsQOp q == "Nil" = Con "Nil" []
 | otherwise = App (App (Var $ parseHsQOp q)  (parseHsExp e)) (parseHsExp e')
parseHsExp a@(L.HsApp e e')
 | isConApp a = Con (getConsName a) (getConsArgs a)
 | otherwise = App (parseHsExp e) (parseHsExp e')
parseHsExp (L.HsNegApp e) = App (Var "negate") (parseHsExp e)
parseHsExp (L.HsLambda _ vs e) =
    let
        e' = parseHsExp e
        vs' = map (\(L.HsPVar v) -> parseHsName v) vs
    in foldr (\v'' e'' -> Lambda v'' e'') e' vs'
parseHsExp (L.HsLet decls e) =
    let
        fLets = parseHsDecls decls
        fExpr = parseHsExp e
    in foldl (\e (f', e') -> App (Lambda f' e) e') fExpr fLets
parseHsExp (L.HsIf c t e) = Case (parseHsExp c) [Branch "True" [] $ parseHsExp t, Branch "False" [] $ parseHsExp e]
parseHsExp (L.HsCase e alts) = Case (parseHsExp e) (parseCaseAlts alts)
parseHsExp (L.HsTuple es) = Con "Tuple" (map parseHsExp es)
parseHsExp (L.HsList []) = Con "Nil" []
parseHsExp (L.HsList es) = list2con $ map parseHsExp es
parseHsExp (L.HsParen e) = parseHsExp e
parseHsExp (L.HsExpTypeSig _ e t) = Typed (parseHsExp e) t
parseHsExp e = error $ show e

parseCaseAlts = map parseCaseAlt

parseCaseAlt (L.HsAlt _ pat alt []) =
    let
        p'@(c, es) = parseCasePat pat
        e' = (parseHsGuardedAlts alt)
    in (Branch c es e')

parseHsGuardedAlts (L.HsUnGuardedAlt e) = parseHsExp e
parseHsGuardedAlts (L.HsGuardedAlts es) = parseGuardedAlts es

parseGuardedAlts (L.HsGuardedAlt _ e e':[]) = Case (parseHsExp e) [Branch "True" [] $ parseHsExp e']
parseGuardedAlts (L.HsGuardedAlt _ e e':as) = Case (parseHsExp e) [Branch "True" [] $ parseHsExp e', Branch "False" [] $ parseGuardedAlts as]

parseCasePat (L.HsPApp c es) = (parseHsQName c, map (\(L.HsPVar v) -> parseHsName v) es)
parseCasePat (L.HsPParen p) = parseCasePat p
parseCasePat (L.HsPInfixApp (L.HsPVar e) c (L.HsPVar e')) = (parseHsQName c, [parseHsName e, parseHsName e'])
parseCasePat (L.HsPList []) = ("Nil", [])

isConApp (L.HsApp (L.HsCon _) _) = True
isConApp (L.HsApp e _) = isConApp e
isConApp _ = False

getConsName (L.HsApp (L.HsCon c) _) = parseHsQName c
getConsName (L.HsApp e _) = getConsName e

getConsArgs (L.HsApp (L.HsCon _) e) = [parseHsExp e]
getConsArgs (L.HsApp e e') = getConsArgs e ++ [parseHsExp e']

parseHsQName (L.Qual (L.Module m) n) = m ++ "." ++ parseHsName n
parseHsQName (L.UnQual n) = parseHsName n
parseHsQName (L.Special s) = parseHsSpecialCon s

parseHsSpecialCon (L.HsUnitCon) = "()"
parseHsSpecialCon (L.HsListCon) = "Nil"
parseHsSpecialCon (L.HsFunCon) = "->"
parseHsSpecialCon (L.HsCons) = "Cons"
parseHsSpecialCon _ = error "Unhandled special con"

parseHsQOp (L.HsQVarOp v) = parseHsQName v
parseHsQOp (L.HsQConOp c) = parseHsQName c

parseHsOp (L.HsVarOp v) = parseHsName v
parseHsOp (L.HsConOp c) = parseHsName c