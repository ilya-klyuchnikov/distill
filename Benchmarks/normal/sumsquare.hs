module Main(main) where

import Prelude hiding(enumFromTo, sum, map, True, False, concat, concatMap, subtract)

data Nat = Z | S Nat deriving Show;
data List a = Nil | Cons a (List a) deriving Show
data Bool = True | False deriving Show

main = print $ root 

root = (f x)

f = \x -> sum (concatMap concatMapMul (enumFromTo (S Z) x))

concatMapMul = \l -> map (mul l) (enumFromTo (S Z) l)

sum = \xs -> sumWith Z xs

sumWith = \acc xs -> case xs of
    Nil -> acc
    Cons y ys -> sumWith (plus y acc) ys

concatMap = \g xs -> case xs of
   Nil -> Nil
   Cons y ys -> concat (g y) (concatMap g ys)

concat = \xs ys -> case xs of
   Nil -> ys
   Cons z zs -> Cons z (concat zs ys)

plus = \x y -> case x of
      Z -> y
      S z -> S (plus z y)

mul = \x y -> case y of
      Z -> Z
      S z -> plus (mul x z) x

map = \g xs -> case xs of
      Nil -> Nil
      Cons y ys -> Cons (g y) (map g ys)

enumFromTo = \i j -> case (lessThan j i) of
   True -> Nil
   False -> Cons i (enumFromTo (plus i (S Z)) j)

lessThan = \x y -> case y of
      Z -> case x of
            Z -> False
            S x1 -> False
      S y1 -> case x of
            Z -> True
            S x1 -> lessThan x1 y1