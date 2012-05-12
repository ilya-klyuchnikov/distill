module Main(main) where

data List a = Nil  | Cons a (List a) deriving Show
data Nat  = Z  | S Nat deriving Show

x = (S (S (S (S (S (S Z))))))

main = print (case  x  of 
                    Z  -> Z
                    S t41 -> let f = \z42 u42 -> case  z42  of 
                                                        S s23 -> ((f s23) (S (S u42)))
                                                        Z  -> (S (S u42))
                             in ((f t41) Z))