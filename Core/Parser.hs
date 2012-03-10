module Core.Parser where

import GHC
import GHC.Paths
import DynFlags
import HscTypes
import GhcPlugins
import CoreSyn
import qualified Core.Expr as E
import qualified CoreSyn as CS
import qualified Literal as L
import Debug.Trace
import Text.Regex.Posix
import Data.List.Utils
{- returns a list of bindings -}

parseProgram :: FilePath -> IO CoreProgram
parseProgram fileName = do
  defaultErrorHandler DynFlags.defaultLogAction $
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      compileToCoreSimplified fileName >>= \b -> return $ cm_binds b

-- CoreProgram is [CoreBind], CoreBind is Bind CoreBndr, CoreBndr is Var

parseCoreProgram :: CoreProgram -> E.Program
parseCoreProgram = map parseCoreBind

parseCoreBind :: CoreBind -> E.Function
parseCoreBind (NonRec f e) = (parseVarUnique f, parseBind e)

parseVarUnique :: Var -> String
parseVarUnique v = showSDoc $ ppr $ varUnique v

parseVarName :: Var -> String
parseVarName v = showSDoc $ ppr $ varName v

isRubbishVar v = v =~ ".*\\$.+" :: Bool

parseVar :: String -> E.Expr
parseVar v
 | isRubbishVar v = E.Not
 | otherwise = case replace "GHC.Base." "" v == v of
	True -> E.Var v
	_ -> E.Var $ replace "GHC.Base." "" v

parseBind :: CS.Expr (CoreBndr) -> E.Expr
parseBind (CS.Var v) = parseVar $ parseVarName v
parseBind (CS.Lit l) = E.Lit $ parseLit l
parseBind (CS.App e e') = case parseBind e of
	E.Not -> parseBind e'
	e'' -> case parseBind e' of
		E.Not -> e''
		e''' -> E.App e'' e'''
parseBind (CS.Lam f e) = (E.Lambda (parseVarName f) (parseBind e))
parseBind _ = E.Not

parseLit :: L.Literal -> E.Lit
parseLit (L.MachChar c) = E.Char c
parseLit (L.MachStr s) = E.String $ show s
parseLit (L.MachInt i) = E.Int i
parseLit (L.MachInt64 i) = E.Int i
parseLit (L.MachFloat f) = E.Frac f
parseLit (L.MachDouble d) = E.Frac d
parseLit l = error $ "Unhandled literal type from GHC: " ++ show l