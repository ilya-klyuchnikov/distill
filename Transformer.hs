module Main(main) where

import System (getArgs)
import Parser
import Expr
import Transform
import Exception
import Context
import Debug.Trace
    
main = do
    args <- getArgs
    case length args == 2 of
        False -> error "Insufficient arguments:\nUsage: transform <super|distill> <filename>"
        True -> do
                    let
                        tType = head args
                        fileName = args !! 1
                    (Program imports dataDecls e fs) <- parseFile ("Benchmarks/inputs/" ++ fileName ++ ".hs")
                    case tType of
                        "super" -> do
                                    let
                                        (NoExn e') = {-trace ((show (Program e fs)) ++ "\n\n") $-} transform 0 e EmptyCtx [] [] (free e) fs
                                        (e'', fs') = residualise e' (free e') [] []
                                    writeFile ("Benchmarks/super/" ++ fileName ++ ".hs") (show (Program imports dataDecls e'' fs'))
                        _ -> error $ "Unsupported transformation: " ++ tType
                