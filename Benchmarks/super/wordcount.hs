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

main = print . f =<< getContents

f = (\(getContents) -> (case getContents of
                         [] -> 0
                         (y:ys) -> (case ys of
                                     [] -> 1
                                     (y:ys) -> (case getContents of
                                                 (y:ys) -> (case y of
                                                             ' ' -> 1 + (f ((case getContents of
                                                                              [] -> []
                                                                              (y:ys) -> ys)))
                                                             _ -> f ((case getContents of
                                                                            [] -> []
                                                                            (y:ys) -> ys)))))))