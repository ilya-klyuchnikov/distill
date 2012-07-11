module Main(main) where

data Nat = Z | S Nat deriving Show

x = S (S (S (S Z)))

main = print root

root = g x x

g y z = case y of
	Z -> h z
	S y' -> g y' z

h z = case z of
	Z -> Z
	S u' -> h u'
