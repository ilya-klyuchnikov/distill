module Core.Parser where

import Core.Expr
import qualified Language.Haskell.Syntax as L
import qualified Language.Haskell.Parser as P
import Data.List (delete)
import Data.Foldable (find)
import Debug.Trace
import Core.Pretty

parseFile f = do
    contents <- readFile f
    let
        m = P.parseModule contents
    case m of
        (P.ParseOk n) ->
            let 
                !funcs = parseHsModule n
                !(funcNames, funcBodies) = unzip $ parseHsModule n
                !fixedFuncs = map (fixFuncs funcNames) funcBodies
                !funcs' = zip funcNames fixedFuncs
            in case find (\(f, e) -> f == "main") funcs' of
                Just (f, e) -> return $ Program e (delete (f, e) funcs)
                _ -> error "No main function found"
        _ -> error "Problem"
        

parseHsModule (L.HsModule _ _ _ _ decls) = parseHsDecls decls

parseHsDecls decls = map parseHsDecl $ filterDecls decls

filterDecls = filter isFuncDecl

isFuncDecl (L.HsPatBind {}) = True
isFuncDecl (L.HsFunBind {}) = True
isFuncDecl _ = False

fixFuncs names e@(Var v)
 | v == "app" = error "herehere"
 | v `elem` names = Func v
 | otherwise = e
fixFuncs names (Con s es) = Con s $ map (fixFuncs names) es
fixFuncs names (Lambda v e) = Lambda v $ fixFuncs names e
fixFuncs names (App e e') = App (fixFuncs names e) (fixFuncs names e')
fixFuncs names (InfixApp e c e') = InfixApp (fixFuncs names e) c (fixFuncs names e')
fixFuncs names (Case e bs) = Case (fixFuncs names e) (map (\(p, e) -> (p, fixFuncs names e)) bs)
fixFuncs names (Typed e t) = Typed (fixFuncs names e) t
fixFuncs names e = trace (show e) e

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
        fExpr = foldr (\v e -> Lambda v (abstract 0 v e)) fBody args
    in (fName, fExpr)
parseHsDecl d = error $ show d
    
parseHsRhs (L.HsUnGuardedRhs e) = parseHsExp e
parseHsRhs (L.HsGuardedRhss es) =
    let
        parsedGuards = map parseHsGuardedRhs es
        parsedExpr = buildGuardCase parsedGuards
    in parsedExpr
    
parseHsGuardedRhs (L.HsGuardedRhs _ c e) = (parseHsExp c, parseHsExp e)

buildGuardCase :: [(Expr, Expr)] -> Expr
buildGuardCase ((c, e):[]) = Case c [(Pattern "True" [], e)]
buildGuardCase ((c, e):es) = Case c [(Pattern "True" [], e), (Pattern "False" [], buildGuardCase es)]

parseHsName (L.HsIdent s) = s
parseHsName (L.HsSymbol s) = s

parseHsExp (L.HsVar v) = Var $ parseHsQName v
parseHsExp (L.HsCon c) = Con (parseHsQName c) []
parseHsExp (L.HsLit l) = Lit l
parseHsExp (L.HsInfixApp e q e') = InfixApp (parseHsExp e) (parseHsQOp q) (parseHsExp e')
parseHsExp a@(L.HsApp e e')
 | isConApp a = Con (getConsName a) (getConsArgs a)
 | otherwise = App (parseHsExp e) (parseHsExp e')
parseHsExp (L.HsNegApp e) = App (Var "negate") (parseHsExp e)
parseHsExp (L.HsLambda _ vs e) =
    let
        e' = parseHsExp e
        vs' = map (\(L.HsPVar v) -> parseHsName v) vs
    in foldr (\v e -> Lambda v (abstract 0 v e)) e' vs'
parseHsExp (L.HsLet decls e) =
    let
        fLets = parseHsDecls decls
        fExpr = parseHsExp e
    in foldl (\e (f', e') -> App (Lambda f' (abstract 0 f' e)) e') fExpr fLets
parseHsExp (L.HsIf c t e) = Case (parseHsExp c) [(Pattern "True" [], parseHsExp t), (Pattern "False" [], parseHsExp e)]
parseHsExp (L.HsCase e alts) = Case (parseHsExp e) (parseCaseAlts alts)
parseHsExp (L.HsTuple es) = Con "Tuple" (map parseHsExp es)
parseHsExp (L.HsList es) = list2con $ map parseHsExp es
parseHsExp (L.HsParen e) = parseHsExp e
parseHsExp (L.HsExpTypeSig _ e t) = Typed (parseHsExp e) t
parseHsExp e = error $ show e

parseCaseAlts = map parseCaseAlt

parseCaseAlt (L.HsAlt _ pat alt decls) =
    let
        p'@(Pattern c es) = parseCasePat pat
        e' = foldl (\e v -> abstract 0 v e) (parseHsGuardedAlts alt) (c:es)
        l' = parseHsDecls decls
    in (p', foldl (\e (f', e') -> App (Lambda f' (abstract 0 f' e)) e') e' l')

parseHsGuardedAlts (L.HsUnGuardedAlt e) = parseHsExp e
parseHsGuardedAlts (L.HsGuardedAlts es) = parseGuardedAlts es

parseGuardedAlts (L.HsGuardedAlt _ e e':[]) = Case (parseHsExp e) [(Pattern "True" [], parseHsExp e')]
parseGuardedAlts (L.HsGuardedAlt _ e e':as) = Case (parseHsExp e) [(Pattern "True" [], parseHsExp e'), (Pattern "False" [], parseGuardedAlts as)]

parseCasePat (L.HsPApp c es) = Pattern (parseHsQName c) (map (\(L.HsPVar v) -> parseHsName v) es)
parseCasePat (L.HsPParen p) = parseCasePat p
parseCasePat (L.HsPInfixApp (L.HsPVar e) c (L.HsPVar e')) = Pattern (parseHsQName c) [parseHsName e, parseHsName e']
parseCasePat (L.HsPList []) = Pattern "[]" []

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
parseHsSpecialCon (L.HsListCon) = "[]"
parseHsSpecialCon (L.HsFunCon) = "->"
parseHsSpecialCon (L.HsCons) = "(:)"
parseHsSpecialCon _ = error "Unhandled special con"

parseHsQOp (L.HsQVarOp v) = parseHsQName v
parseHsQOp (L.HsQConOp c) = parseHsQName c

parseHsOp (L.HsVarOp v) = parseHsName v
parseHsOp (L.HsConOp c) = parseHsName c