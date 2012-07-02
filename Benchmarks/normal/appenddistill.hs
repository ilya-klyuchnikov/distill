module Main(main) where


 
data Nat = Z
         | S Nat
         deriving Show

 
data List a = Nil
            | Cons a (List a)
            deriving Show

main = root

root = print ((case xs of
                [] -> []
                (y:(ys)) -> f (ys) ((y:[]))))

f = (\(ys) (x) -> (case ys of
                    [] -> x
                    (y':(ys'')) -> f (ys'') ((y':(x)))))