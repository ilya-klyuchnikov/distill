module Main(main) where


import Prelude hiding (flip)

 
data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving Show

 
data Nat = Z
         | S Nat
         deriving Show

x = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))))

main = print $ root

root = (case x of
         Z -> S (Z)
         S (o) -> (case o of
                    Z -> S (S (Z))
                    S (o') -> f (o')))

f = (\(o') -> (case o' of
                Z -> S (S (S (S (Z))))
                S (o'') -> f (o'')))
