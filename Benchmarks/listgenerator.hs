module Main(main) where

import System (getArgs)

data List a = Nil | Cons a (List a) deriving Show
data Nat = Z | S Nat deriving Show

main = do
    args <- getArgs
    let i = read (head args) :: Int
    print (listFromInt i)
    
natFromInt i
 | i <= 0 = Z
 | otherwise = S (natFromInt (i - 1))

listFromInt i = Cons (S Z) (listFromInt' (i - 1))

listFromInt' i
 | i == 0 = Nil
 | otherwise = Cons Z (listFromInt' (i - 1))
 