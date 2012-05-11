module Main(main) where
	
import Prelude hiding (head, tail, length)

main = print . linecount =<< getContents

linecount = \xs -> case (length xs) of
   0 -> 0;
   z -> case (z - 1) of
      0 -> 1
      _ -> case head xs of
         '\n' -> 1 + (linecount (tail xs))
         _ -> linecount (tail xs)

head = \xs -> case xs of
   (y:ys) -> y

tail = \xs -> case xs of
	[] -> []
	(y:ys) -> ys

length = \xs -> case xs of
   [] -> 0
   (y:ys) -> 1 + (length ys)