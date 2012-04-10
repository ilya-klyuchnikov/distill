module Main(main) where

data Nat = Z | S Nat

main = print $ (root (9::Int) :: Int)

root n = int (fromInteger_ 3 ^^^ fromInteger_ n)

int :: Nat -> Int
int x = case x of
    Z     -> 0
    (S x) -> 1 + int x

fromInteger_ x = if x < 1 then Z else S (fromInteger_ (x-1))

x ^^^ y = case y of
    Z   -> S Z
    S y -> x *& (x ^^^ y)

x *& y = case y of
    Z -> Z
    S y -> (x *& y) +& x
    
x +& y = case x of
    Z -> y
    S x -> S (x +& y)