module Main(main) where
	
import Prelude hiding (subtract, True, False)	
	
data Nat = Z | S Nat deriving Show
data Bool = True | False deriving Show

main = print (nfib x)

nfib = \x -> case (lessThanEq x (S Z)) of
   True -> S Z
   False -> plus (plus (nfib (subtract x (S Z))) (nfib (subtract x (S (S Z))))) (S Z)

lessThanEq = \x y -> case y of
      Z -> case x of
            Z -> True
            S x1 -> False
      S y1 -> case x of 
            Z -> True
            S x1 -> lessThanEq x1 y1

subtract = \x y -> case y of
      Z -> x
      S y1 -> case x of
            Z -> Z
            S x1 -> subtract x1 y1

plus = \x y -> case x of
      Z -> y
      S z -> S (plus z y)

x = fromInt 35

fromInt x = if x < 1 then Z else S (fromInt (x-1))