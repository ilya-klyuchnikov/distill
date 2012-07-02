module Main(main) where


 
data Nat = Z
         | S Nat
         deriving Show

x = (S (S (S Z)))

main = print root

root = f (x)

f = (\(x) -> (case x of
               Z -> S (Z)
               S (o) -> f (o)))
