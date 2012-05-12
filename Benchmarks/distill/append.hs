module Main(main) where


 
data Nat = Z
         | S Nat
         deriving Show

 
data List a = Nil
            | Cons a (List a)
            deriving Show

main = print ((case xs of
                [] -> []
                (y:ys) -> f (ys) ([]) (y)))

f = (\(ys) (x) (y) -> (case ys of
                        [] -> (y:x)
                        (y':ys) -> f (ys) ((y:x)) (y')))