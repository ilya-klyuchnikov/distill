module Main(main) where

data List a = Nil | Cons a (List a) deriving Show
data Nat = Z | S Nat deriving Show

main = print (charcount (getContents))

charcount = \xs -> case xs of
      Nil -> Z
      Cons y ys -> S (charcount ys)