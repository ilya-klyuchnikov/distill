{-# OPTIONS_GHC -fffi #-}

import Foreign.C.Types
import System.IO
import System.IO.Unsafe


main = print . lineCount =<< getContents

foreign import ccall unsafe "stdio.h getchar" getchar :: IO CInt

lineCount xs
 | head xs == '\n' = 1 + (lineCount (tail xs))
 | length xs == 1 = 1
 | otherwise = linecount (tail xs)
