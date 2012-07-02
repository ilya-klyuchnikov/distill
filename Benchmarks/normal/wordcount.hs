module Main(main) where
	
import Prelude hiding (head, tail, length)

data Nat = Z | S Nat deriving Show
data List a = Nil | Cons a (List a) deriving Show
data Char = Space | Letter deriving Show

main = print (wordcount getContents)

wordcount = \xs -> case (length xs) of
   Z -> Z
   S y -> case y of
      Z -> S Z
      S z -> case (head xs) of
         Space -> S (wordcount (tail xs))
         Letter -> wordcount (tail xs)

head = \xs -> case xs of
   Cons y ys -> y

tail = \xs -> case xs of
   Nil -> Nil
   Cons y ys -> ys

length = \xs -> case xs of
   Nil -> Z
   Cons y ys -> S (length ys)
