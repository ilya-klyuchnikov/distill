-- This is how we will compile the transformed output eventually.

module Core.Compile where

import GHC
import GHC.Paths
import DynFlags
import HscTypes
import GhcPlugins
import CoreSyn
import Debug.Trace
import Text.Regex.Posix
import Data.List.Utils
{- returns a list of bindings -}

parseProgram fileName = do
  defaultErrorHandler DynFlags.defaultLogAction $
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags