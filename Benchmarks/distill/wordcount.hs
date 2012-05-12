module Main(main) where


import Prelude hiding (head, tail, length)

 
data Nat = Z
         | S Nat
         deriving Show

 
data List a = Nil
            | Cons a (List a)
            deriving Show

 
data Char = Space
          | Letter
          deriving Show

main = print (f (getContents))

f = (\(getContents) -> (case getContents of
                         [] -> Z
                         (y:ys) -> (case ys of
                                     [] -> S (Z)
                                     (y':ys) -> (case y of
                                                  Space -> S (f (tail ((Space:(y':ys)))))
                                                  Letter -> f (tail ((Letter:(y':ys))))))))