module Main(main) where
	
data Nat = Z | S Nat deriving Show

main = print root

root = (fac x)

x = S (S (S Z))

plus = \x y -> case x of
      Z -> y
      S z -> S (plus z y)

mul = \x y -> case y of
      Z -> Z
      S z -> plus (mul x z) x

fac = \n -> case n of
      Z -> S Z
      S o -> mul n (fac o)