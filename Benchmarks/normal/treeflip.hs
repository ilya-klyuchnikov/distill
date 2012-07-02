module Main(main) where

import Prelude hiding (flip)	

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show
data Nat = Z | S Nat deriving Show

main = print $ root

root = (sumtr (flip (flip (buildTree x (Leaf (S Z))))))

flip = \t -> case t of
    Leaf x -> Leaf x
    Branch l r -> Branch (flip l) (flip r)

sumtr = \t -> case t of
    Leaf x -> x
    Branch l r -> plus (sumtr l) (sumtr r)

buildTree = \n t -> case n of
    Z -> t
    S o -> buildTree o (Branch t t)

plus = \x y -> case x of
      Z -> y
      S z -> S (plus z y)