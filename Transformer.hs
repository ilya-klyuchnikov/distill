module Main(main) where

import System.Environment (getArgs)
import Parser
import Term
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
                                        u = super e EmptyCtx (free e) [] fs
                                        (t'', fs') = residualise u [] []
                                    writeFile outputFile (show (Program imports dataDecls main t'' fs'))
                        "distill" -> do
                                      let
                                          t' = drive (addLetrecs [] fs e) EmptyCtx (free e) [] []
                                          u = distill t' (free t') []
                                          (u', fs') = residualise u [] []
                                      writeFile outputFile (show (Program imports dataDecls main u' fs'))
                        _ -> error $ "Unsupported transformation: " ++ tType
                
