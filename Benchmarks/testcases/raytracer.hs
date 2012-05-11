module Main(main) where

import Prelude hiding (zipWith, sum, replicate)

data List a = Nil | Cons a (List a) deriving Show
data Nat = Z | S Nat deriving Show

main = print (root ((replicate n (S Z))) ((replicate n (S (S Z)))))

plus = \x y -> case x of
      Z -> y
      S z -> S (plus z y)

mul = \x y -> case y of
      Z -> Z
      S z -> plus (mul x z) x

zipWith = \f xs ys -> case xs of
   Nil -> Nil
   Cons a as -> case ys of
      Nil -> Nil
      Cons b bs -> Cons (f a b) (zipWith f as bs)


sum = \xs -> sumWith Z xs

sumWith = \acc xs -> case xs of
    Nil -> acc
    Cons y ys -> sumWith (plus y acc) ys

root = \xs ys -> sum (zipWith mul xs ys)

replicate = \n x -> case n of
   Z -> Nil
   S o -> case o of
      Z -> Cons x Nil
      S p -> Cons x (replicate o x)

n = (S (S (S (S (S (S Z))))))