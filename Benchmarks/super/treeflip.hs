module Main(main) where


import Prelude hiding (flip)

 
data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving Show

 
data Nat = Z
         | S Nat
         deriving Show

x = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))))

main = print (f''' (x) (Leaf (S (Z))))

f''' = (\(x) (x') -> (case x of
                       Z -> (case x' of
                              Leaf (x) -> x
                              Branch (l) (r) -> f'''' (l) (r))
                       S (o) -> f''' (o) (Branch (x') (x'))))

f'''' = (\(l) (r) -> (case (case l of
                             Leaf (x) -> x
                             Branch (l) (r) -> f'''' (l) (r)) of
                       Z -> (case r of
                              Leaf (x) -> x
                              Branch (l) (r) -> f'''' (l) (r))
                       S (z) -> S (f''''''' (z) (r))))

f''''''' = (\(z) (r) -> (case z of
                          Z -> f'''''''' (r)
                          S (z) -> S (f''''''' (z) (r))))

f'''''''' = (\(r) -> (case r of
                       Leaf (x) -> x
                       Branch (l) (r) -> f''''''' (f'''''''' (l)) (r)))