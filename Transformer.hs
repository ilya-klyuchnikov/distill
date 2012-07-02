module Main(main) where

import System.Environment (getArgs)
import Parser
import Expr
import Transform
import Exception
import Context
import Debug.Trace
import System.FilePath
    
main = do
    args <- getArgs
    case length args == 3 of
        False -> error "Insufficient arguments:\nUsage: transform <super|distill> <filename> <output_file_append>"
        True -> do
                    let
                      tType = args !! 0
                      fileName = args !! 1
                      outputAppend = args !! 2
                      (dir, file) = splitFileName fileName
                      inputDir = if dir == "./" then "Benchmarks/" else dir
                      inputFile = inputDir ++ (if dir == "./" then "normal/" else "") ++ file
                      outputFile = inputDir ++ (dropExtension file) ++ outputAppend ++ ".hs"
                    (Program imports dataDecls main e fs) <- parseFile inputFile
                    case tType of
                      "super" -> do
                                  let
                                      (NoExn e') = {-trace ((show (Program e fs)) ++ "\n\n") $-} transform 0 e EmptyCtx [] [] (free e) fs
                                      (e'', fs') = residualise e' (free e') [] []
                                  writeFile outputFile (show (Program imports dataDecls main e'' fs'))
                      _ -> error $ "Unsupported transformation: " ++ tType
                
