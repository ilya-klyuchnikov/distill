module Main(main) where


import Prelude hiding (zipWith, sum, replicate)

 
data List a = Nil
            | Cons a (List a)
            deriving Show

 
data Nat = Z
         | S Nat
         deriving Show

main = print ((case n of
                Z -> Z
                S (o) -> (case o of
                           Z -> S (S (Z))
                           S (p) -> f (p) (Z))))

f = (\(p) (x) -> (case p of
                   Z -> S (S (S (S (x))))
                   S (p) -> f (p) (S (S (x)))))
                   
n = fromInt 6000000

fromInt x = if x < 1 then Z else S (fromInt (x-1))