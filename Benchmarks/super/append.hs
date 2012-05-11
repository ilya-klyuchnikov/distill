module Main(main) where
  
data Nat = Z
         | S Nat
         deriving Show

                          
data List a = Nil
            | Cons a (List a)
            deriving Show

main = print ([Z,S (Z),S (S (Z)),S (S (S (Z)))])