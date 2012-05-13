module Main(main) where

import System(getArgs)

data Nat = Z | S Nat deriving Show
    
main = do
    args <- getArgs
    let i = read (head args) :: Int
    print (fromInt i)
    
fromInt x = if x < 1 then Z else S (fromInt (x-1))