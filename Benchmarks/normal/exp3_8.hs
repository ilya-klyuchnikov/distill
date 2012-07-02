module Main(main) where

data Nat = Z | S Nat deriving Show

main = print $ root

root = pow (S (S (S Z))) x

plus = \x y -> case x of
      Z -> y
      S z -> S (plus z y)

mul = \x y -> case y of
      Z -> Z
      S z -> plus (mul x z) x

pow = \x y -> case y of
      Z -> S Z
      S z -> mul x (pow x z)
