module Main(main) where

import System (getArgs)
import Core
import Core.Parser
import Core.Term
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
                    (Program e fs) <- parseFile ("Benchmarks/" ++ fileName ++ ".hs")
                   
                    case tType of
                        "super" -> do
                                    let
                                        (NoExn e') = trace ((show (Program e fs)) ++ "\n\n") $ transform 0 e EmptyCtx [] [] (free e) fs
                                        (e'', fs') = residualise e' (free e') [] []
                                    putStrLn (show (Program e'' fs'))
                        _ -> error $ "Unsupported transformation: " ++ tType
                