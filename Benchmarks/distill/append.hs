module Main(main) where


 
data Nat = Z
         | S Nat
         deriving Show

 
data List a = Nil
            | Cons a (List a)
            deriving Show

main = print ((case xs of
                Nil -> Nil
                Cons y ys -> f (ys) (Nil) (y)))

f = (\(ys) (x) (y) -> (case ys of
                        Nil -> Cons y x
                        Cons y' ys -> f (ys) (Cons y x) (y')))
                        
xs = listFromInt 30000

listFromInt i = Cons (S Z) (listFromInt' (i - 1))

listFromInt' i
 | i == 0 = Nil
 | otherwise = Cons Z (listFromInt' (i - 1))