module Main(main) where


import Prelude hiding (head, tail, length)

 
data List a = Nil
            | Cons a (List a)
            deriving Show

 
data Char = Letter
          | NewLine
          deriving Show

 
data Nat = Z
         | S Nat
         deriving Show

main = print . f =<< getContents

f = (\(getContents) -> (case getContents of
                         [] -> Z
                         (y:ys) -> (case ys of
                                     [] -> S (Z)
                                     (y':ys) -> (case y of
                                                  '\n' -> S (f ((y':ys)))
                                                  _ -> f ((y':ys))))))