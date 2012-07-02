module Main(main) where


 
data Nat = Z
         | S Nat
         deriving Show

 
data List a = Nil
            | Cons a (List a)
            deriving Show

main = root

root = print (f (xs))

f = (\(xs) -> (case xs of
                Nil -> ()
                Cons (y) (ys) -> (case f (ys) of
                                   Nil -> (y)
                                   Cons (z) (zs) -> Cons (z) (f'' (zs) (y)))))

f'' = (\(zs) (y) -> (case zs of
                      Nil -> (y)
                      Cons (z) (zs) -> Cons (z) (f'' (zs) (y))))