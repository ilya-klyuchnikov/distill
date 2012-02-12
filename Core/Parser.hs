module Core.Parser where

import Core.Expr

import qualified Language.Haskell.Parser as LHP
import qualified Language.Haskell.Syntax as LHS
import System.FilePath
import Text.PrettyPrint.HughesPJ
import Control.Arrow hiding ((<+>))
import Data.List (nub, find, (\\))


{-|
    Given a filepath, generates a program representing that file.

    [@file@] The file to be parsed into a program.
-}

parseFile :: FilePath -> IO Prog
parseFile file = do
    fileContents <- readFile file
    case LHP.parseModule fileContents of
            LHP.ParseOk parsedModule -> return (parseModule parsedModule)
            LHP.ParseFailed srcLoc _ -> error $ "Parse failed at: "  ++ file ++ ": " ++ show srcLoc

{-|
    Given a module, generates a program.

    [@module@] The module to be parsed into a program.

-}

parseModule :: LHS.HsModule -> Prog
parseModule (LHS.HsModule _ _ _ _ decls) =
    let
        funcNames = gatherFuncNames decls -- gather initial function names
        parsedExprs = parseDecls funcNames decls -- convert decls to expressions
    in case find (\f -> fst f == "main") parsedExprs of
        Just a -> Prog (snd a) (parsedExprs \\ [a])
        Nothing -> error "No main function defined"

{-|
    Given a set of known functions, and a set of declarations, generates a set of functions.

    [@funcNames@] List of currently known functions (allows for fixing of var to func)

    [@decls@] List of declarations.
-}

parseDecls :: [String] -> [LHS.HsDecl] -> [Function]
parseDecls funcNames decls = map (parseDecl funcNames) (filter isFuncDecl decls)

{-|
    Given a set of known functions, and a declaration, generates a function.

    [@funcNames@] List of currently known functions (allows for fixing of var to func)

    [@decl@] The declaration to generate a function from.

    NOTE: Does not allow functions with multiple matches.
-}

parseDecl :: [String] -> LHS.HsDecl -> Function
parseDecl funcNames (LHS.HsFunBind [LHS.HsMatch _ fName vars rhs boundFuncs]) =
    let
        name = parseName fName
        args = map parseArgPat vars -- parse function arguments
        boundFuncNames = gatherFuncNames boundFuncs
        parsedBoundFuncs = parseDecls (funcNames ++ boundFuncNames) boundFuncs -- extract bound functions, names used later for correncting function variables in the body
        body = parseRhs (funcNames ++ boundFuncNames) rhs
        fixedBody = foldr Lambda (foldl (\e (boundName, boundExpr) -> App (Lambda boundName e) boundExpr) body parsedBoundFuncs) args
    in (name, fixedBody)
parseDecl funcNames (LHS.HsPatBind _ patName rhs boundFuncs) =
    let
        name = (parseArgPat patName)
        boundFuncNames = gatherFuncNames boundFuncs
        parsedBoundFuncs = parseDecls (funcNames ++ boundFuncNames) boundFuncs
        body = parseRhs (funcNames ++ boundFuncNames) rhs
        fixedBody = foldl (\e (boundName, boundExpr) -> App (Lambda boundName e) boundExpr) body parsedBoundFuncs
    in (name, fixedBody)
parseDecl _ d = error $ "Unsupported decl: " ++ show d

{-|
    Determines whether or not a given declaration defines a function.
-}

isFuncDecl :: LHS.HsDecl -> Bool
isFuncDecl (LHS.HsFunBind {}) = True
isFuncDecl (LHS.HsPatBind {}) = True
isFuncDecl _ = False

{-|
    Given a set of known functions, and a right-hand-side generates an expression.

    [@funcNames@] List of currently known functions (allows for fixing of var to func)
    
    [@rhs@] The Rhs to generate an expression from.
            Guarded Rhs' generates a case expression with each guard yielding a True/False expression, and nesting the next guard.
-}

parseRhs :: [String] -> LHS.HsRhs -> Expr
parseRhs funcNames (LHS.HsUnGuardedRhs e) = parseExp funcNames e
parseRhs funcNames (LHS.HsGuardedRhss rhss) = buildCaseFromGuard $ map (parseGuardedRhs funcNames) rhss

{-|
    Parses guarded Rhses into case bound expressions.
-}

parseGuardedRhs :: [String] -> LHS.HsGuardedRhs -> (Expr, Expr)
parseGuardedRhs funcNames (LHS.HsGuardedRhs _ c e) = (parseExp funcNames c, parseExp funcNames e)


buildCaseFromGuard :: [(Expr, Expr)] -> Expr
buildCaseFromGuard ((c, e):[]) = Case c [(Pattern "True" [], e)]
buildCaseFromGuard ((c, e):es) = Case c [(Pattern "True" [], e),(Pattern "False" [], buildCaseFromGuard es)]

{-|
    Parses Exp to Expr
-}

parseExp :: [String] -> LHS.HsExp -> Expr
parseExp funcNames (LHS.HsVar v)
 | var `elem` funcNames = Func var -- is it a function call?
 | otherwise = Var var
    where
        var = parseQName v 
parseExp _ (LHS.HsCon c) = Con (parseQName c) []
parseExp funcNames a@(LHS.HsApp e e')
 | isConstructorApp a = buildConstructorApp funcNames a
 | otherwise = App (parseExp funcNames e) (parseExp funcNames e')
parseExp funcNames (LHS.HsInfixApp e q e') = InfixApp (parseQOpToString q) (parseExp funcNames e) (parseExp funcNames e')
parseExp funcNames (LHS.HsLambda _ ps e) = foldr (Lambda . parsePatLam) (parseExp funcNames e) ps -- NOTE: Currently only supports variable patterns
parseExp funcNames (LHS.HsLet boundFuncs e) = -- converts Lets to lambdas
    let
        boundFuncNames = gatherFuncNames boundFuncs
        parsedBoundFuncs = parseDecls (funcNames ++ boundFuncNames) boundFuncs
        body = parseExp (funcNames ++ boundFuncNames) e
    in foldl (\e (boundName, boundExpr) -> App (Lambda boundName e) boundExpr) body parsedBoundFuncs
parseExp funcNames (LHS.HsIf c t e) = Case (parseExp funcNames c) [(Pattern "True" [], parseExp funcNames t), (Pattern "False" [], parseExp funcNames e)]
parseExp funcNames (LHS.HsList []) = Con "Nil" []
parseExp funcNames (LHS.HsList ls) = Con "Cons" (map (parseExp funcNames) ls)
parseExp funcNames (LHS.HsParen e) = Paren (parseExp funcNames e)
parseExp funcNames (LHS.HsCase e ps) = Case (parseExp funcNames e) (parseAltsCase funcNames ps)
parseExp _ (LHS.HsLit l) = Lit (parseLit l)
parseExp _ e = error $ "Unsupported expression: " ++ show e

parseLit :: LHS.HsLiteral -> Lit
parseLit (LHS.HsChar c) = Char c
parseLit (LHS.HsString s) = String s
parseLit (LHS.HsInt i) = Int i
parseLit (LHS.HsFrac f) = Frac f


{-|
    Parses alts for case branches.
    NOTE: Currently only supports constructor branch selectors.
-}

parseAltsCase :: [String] -> [LHS.HsAlt] -> [Branch]
parseAltsCase funcNames = map (parseAltCase funcNames)

parseAltCase :: [String] -> LHS.HsAlt -> Branch
parseAltCase funcNames (LHS.HsAlt _ aPattern expr boundFuncs) =
    let
        pattern = parseCasePattern aPattern
        boundFuncNames = gatherFuncNames boundFuncs
        parsedBoundFuncs = parseDecls (funcNames ++ boundFuncNames) boundFuncs
        body = parseCaseExp (funcNames ++ boundFuncNames) expr
        fixedBody = foldl (\e (boundName, boundExpr) -> App (Lambda boundName e) boundExpr) body parsedBoundFuncs
    in (pattern, fixedBody)

{-|
    Parses patterns for case branches.
    NOTE: Currently only parses constructor patterns.
-}

parseCasePattern :: LHS.HsPat -> Pattern
parseCasePattern (LHS.HsPApp c as) = Pattern (parseQName c) (map (\(LHS.HsPVar v) -> parseName v) as)
parseCasePattern p = error $ "Unsupported case pattern: " ++ show p

parseCaseExp :: [String] -> LHS.HsGuardedAlts -> Expr
parseCaseExp funcNames (LHS.HsUnGuardedAlt e) = parseExp funcNames e
parseCaseExp funcNames (LHS.HsGuardedAlts as) = parseGuardedAlts funcNames as

parseGuardedAlts :: [String] -> [LHS.HsGuardedAlt] -> Expr
parseGuardedAlts funcNames (LHS.HsGuardedAlt _ c e:a@(LHS.HsGuardedAlt {}):as) = Case (parseExp funcNames c) [(Pattern "True" [], parseExp funcNames e), (Pattern "False" [], parseGuardedAlts funcNames (a:as))]
parseGuardedAlts funcNames (LHS.HsGuardedAlt _ c e:[]) = Case (parseExp funcNames c) [(Pattern "True" [], parseExp funcNames e)]

{-|
    Parses patterns for lambda expressions.
    NOTE: Currently only supports variable patterns.
-}

parsePatLam :: LHS.HsPat -> String
parsePatLam (LHS.HsPVar var) = parseName var
parsePatLam _ = error "Invalid lambda pattern"

parseQOpToString :: LHS.HsQOp -> String
parseQOpToString (LHS.HsQVarOp v) = parseQName v
parseQOpToString (LHS.HsQConOp c) = parseQName c

{-
    Builds a constructor app.
-}

buildConstructorApp :: [String] -> LHS.HsExp -> Expr
buildConstructorApp funcNames e = buildConstructorApp' funcNames e []

buildConstructorApp' :: [String] -> LHS.HsExp -> [Expr] -> Expr
buildConstructorApp' funcNames (LHS.HsApp e e') as = buildConstructorApp' funcNames e (parseExp funcNames e':as)
buildConstructorApp' _ (LHS.HsCon c) as = Con (parseQName c) as

{-
    Checks to see if an App is a constructor.
-}

isConstructorApp :: LHS.HsExp -> Bool
isConstructorApp (LHS.HsParen e) = isConstructorApp e
isConstructorApp (LHS.HsApp e _) = isConstructorApp e
isConstructorApp (LHS.HsCon _ ) = True
isConstructorApp _ = False

{-|
    Parses function arguments.
    

    NOTE: 

        * Only variable arguments supported e.g. f a b c
        
        * Records etc. disallowed.
-}

parseArgPat :: LHS.HsPat -> String
parseArgPat (LHS.HsPVar var) = parseName var
parseArgPat p = error $ "Unsupported function argument: " ++ show p

{-|
    Gathers all initial function names, to be used for fixing function calls.
-}

gatherFuncNames :: [LHS.HsDecl] -> [String]
gatherFuncNames (LHS.HsFunBind [LHS.HsMatch _ name _ _ _]:ds) = parseName name : gatherFuncNames ds
gatherFuncNames (_:ds) = gatherFuncNames ds
gatherFuncNames [] = []

parseName :: LHS.HsName -> String
parseName (LHS.HsIdent s) = s
parseName (LHS.HsSymbol s) = s

parseQName :: LHS.HsQName -> String
parseQName (LHS.Qual (LHS.Module m) n) = m ++ parseName n
parseQName (LHS.UnQual n) = parseName n
parseQName (LHS.Special s) = parseSpecialCon s

parseSpecialCon :: LHS.HsSpecialCon -> String
parseSpecialCon LHS.HsUnitCon = "()"
parseSpecialCon LHS.HsListCon = "[]"
parseSpecialCon LHS.HsFunCon = "->"
parseSpecialCon (LHS.HsTupleCon i) = "(" ++ take i [',',','..] ++ ")"
parseSpecialCon LHS.HsCons = "(:)"