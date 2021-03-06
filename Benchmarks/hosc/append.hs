module Main(main) where

data List a = Nil  | Cons a (List a) deriving Show
data Nat = Z | S Nat deriving Show

xs = listFromInt 30000

listFromInt i = Cons (S Z) (listFromInt' (i - 1))

listFromInt' i
 | i == 0 = Nil
 | otherwise = Cons Z (listFromInt' (i - 1))

main = print (let f = \y3 -> case  y3  of
                                    Nil  -> Nil
                                    Cons p2 r2 -> let g = \z3 -> case  z3  of 
                                                                        Cons z1 z2 -> (Cons z1 (g z2))
                                                                        Nil  -> (Cons p2 Nil)
                                                  in g (f r2)
              in f xs)