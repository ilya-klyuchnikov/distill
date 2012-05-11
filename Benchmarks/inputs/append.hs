module Main(main) where

data Nat = Z | S Nat deriving Show
data List a = Nil | Cons a (List a) deriving Show

main = print (nrev xs)

nrev = \xs -> case xs of 
      Nil -> Nil
      Cons y ys -> app (nrev ys) (Cons y Nil)

app = \xs ys -> case xs of
      Nil -> ys
      Cons z zs -> Cons z (app zs ys)