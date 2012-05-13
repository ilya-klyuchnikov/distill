module Main(main) where


 
data List a = Nil
            | Cons a (List a)
            deriving Show

 
data Nat = Z
         | S Nat
         deriving Show

main = print . f =<< getContents

f = (\(getContents) -> (case getContents of
                         [] -> 0
                         (y:ys) -> 1 + (f (ys))))