module Main(main) where
	
import Prelude hiding (head, tail, length)

main = print . wordcount =<< getContents

wordcount = \xs -> case (length xs) of
   0 -> 0
   n -> case (n - 1) of
      0 -> 1
      _ -> case (head xs) of 
         ' ' -> 1 + (wordcount (tail xs))
         _ -> wordcount (tail xs)

head = \xs -> case xs of
   (y:ys) -> y;

tail = \xs -> case xs of
   [] -> []
   (y:ys) -> ys

length = \xs -> case xs of
   [] -> 0
   (y:ys) -> 1 + (length ys)
