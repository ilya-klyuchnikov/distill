module Main(main) where
 
data Nat = Z
         | S Nat
         deriving Show

main = print (S (S (S (S (S (S (Z)))))))