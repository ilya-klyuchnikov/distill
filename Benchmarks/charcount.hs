{-# LANGUAGE ForeignFunctionInterface #-}

module Main(main) where

import Foreign.C.Types
import System.IO
import System.IO.Unsafe

main = print . charcount =<< getContents

charcount xs
 | length xs == 1 = 1
 | otherwise = 1 + (charcount (tail xs))