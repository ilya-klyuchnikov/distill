module Main(main) where
	
import Prelude hiding (head, tail, length)

data List a = Nil | Cons a (List a) deriving Show
data Char = Letter | NewLine deriving Show
data Nat = Z | S Nat deriving Show

main = print (linecount getContents)

linecount = \xs -> case (length xs) of
   Z -> Z
   S z -> case z of
      Z -> S Z
      S a -> case head xs of 
         NewLine -> S (linecount (tail xs))
         Letter -> linecount (tail xs)

head = \xs -> case xs of
   Cons y ys -> y

tail = \xs -> case xs of
   Nil -> Nil
   Cons y ys -> ys

length = \xs -> case xs of
   Nil -> Z
   Cons y ys -> S (length ys)