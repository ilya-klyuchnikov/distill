module Core.Parser where

import Core.Expr
import qualified Language.Haskell.Syntax as L

parseModule (L.HsModule _ _ _ _ decls) = parseHsDecls decls

parseHsDecls decls = map parseHsDecl decls

parseHsDecl (L.HsPatBind _ (L.HsPVar name) e decls) =
    let
        fName = parseHsName name
        fBody = parseHsRhs e
        fWheres = parseHsDecls decls
    in (fName, LetRec fWheres fBody)
parseHsDecl (L.HsFunBind [L.HsMatch _ name pats e decls]) =
    let
        args = map (\(L.HsPVar v) -> parseHsName v) pats
        fName = parseHsName name
        fBody = parseHsRhs e
        fWheres = parseHsDecls decls
        lExpr = LetRec fWheres fBody
        fExpr = foldl (\e v -> Lambda v e) lExpr args
    in (fName, LetRec fWheres fExpr)
    
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
parseHsExp (L.HsInfixApp e q e') = Con (parseHsQOp q) [parseHsExp e, parseHsExp e']
parseHsExp a@(L.HsApp e e')
 | isConApp a = buildConstructor a
 | otherwise = App (parseHsExp e) (parseHsExp e')
parseHsExp (L.HsNegApp e) = App (Var "negate") (parseHsExp e)
parseHsExp (L.HsLambda _ vs e) =
    let
        e' = parseHsExp e
        vs' = map (\(L.HsPVar v) -> parseHsName v) vs
    in foldl (\e v -> Lambda v e) e' vs'
parseHsExp (L.HsLet decls e) =
    let
        fLets = parseHsDecls decls
        fExpr = parseHsExp e
    in foldl (\e (f', e') -> App (Lambda f' e) e') fExpr fLets
parseHsExp (L.HsIf c t e) = Case (parseHsExp c) [(Pattern "True" [], parseHsExp t), (Pattern "False" [], parseHsExp e)]
parseHsExp (L.HsCase e alts) = Case (parseHsExp e) (parseCaseAlts alts)
parseHsExp (L.HsTuple es) = Con "Tuple" (map parseHsExp es)
parseHsExp (L.HsList es) = Con "List" (map parseHsExp es)
parseHsExp (L.HsParen e) = parseHsExp e
parseHsExp e = error $ show e

parseCaseAlts = map parseCaseAlt

parseCaseAlt (L.HsAlt _ pat alt decls) =
    let
        p' = parseCasePat pat
        e' = parseHsGuardedAlts alt
        l' = parseHsDecls decls
    in (p', foldl (\e (f', e') -> App (Lambda f' e) e') e' l')

parseHsGuardedAlts (L.HsUnGuardedAlt e) = parseHsExp e
parseHsGuardedAlts (L.HsGuardedAlts es) = parseGuardedAlts es

parseGuardedAlts ((L.HsGuardedAlt _ e e'):[]) = Case (parseHsExp e) [(Pattern "True" [], parseHsExp e')]
parseGuardedAlts ((L.HsGuardedAlt _ e e'):as) = Case (parseHsExp e) [(Pattern "True" [], parseHsExp e'), (Pattern "False" [], parseGuardedAlts as)]

parseCasePat (L.HsPApp c es) = Pattern (parseHsQName c) (map (\(L.HsPVar v) -> parseHsName v) es)
parseCasePat (L.HsPParen p) = parseCasePat p

buildConstructor e = Con (getConsName e) (getConsArgs e)

isConApp (L.HsApp (L.HsCon _) _) = True
isConApp (L.HsApp e _) = isConApp e
isConApp _ = False

getConsName (L.HsApp (L.HsCon c) _) = parseHsQName c
getConsName (L.HsApp e _) = getConsName e

getConsArgs (L.HsApp (L.HsCon _) e) = [parseHsExp e]
getConsArgs (L.HsApp e e') = (getConsArgs e) ++ [parseHsExp e']

parseHsQName (L.Qual (L.Module m) n) = m ++ "." ++ parseHsName n
parseHsQName (L.UnQual n) = parseHsName n
parseHsQName (L.Special s) = parseHsSpecialCon s

parseHsSpecialCon (L.HsUnitCon) = "()"
parseHsSpecialCon (L.HsListCon) = "[]"
parseHsSpecialCon (L.HsFunCon) = "->"
parseHsSpecialCon (L.HsCons) = "(:)"
parseHsSpecialCon _ = error $ "Unhandled special con"

parseHsQOp (L.HsQVarOp v) = parseHsQName v
parseHsQOp (L.HsQConOp c) = parseHsQName c

parseHsOp (L.HsVarOp v) = parseHsName v
parseHsOp (L.HsConOp c) = parseHsName c