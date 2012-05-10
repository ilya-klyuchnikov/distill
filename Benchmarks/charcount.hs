module Main(main) where

import Prelude hiding (tail, length)

main = print . charcount =<< getContents

charcount xs
 | length xs == 1 = 1
 | otherwise = 1 + (charcount (tail xs))

tail xs = case xs of
	[] -> []
	(x:xs) -> xs
	
length xs = case xs of
	[] -> 0
	(x:xs) -> length' 1 xs

length' l xs = case xs of
	[] -> l
	(x:xs) -> length' (l + 1) xs
	
	