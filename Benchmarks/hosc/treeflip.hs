module Main(main) where

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show
data Nat  = Z  | S Nat deriving Show

x = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))))

main = print (let f = \w18 p18 -> case  w18  of 
                                        S r11 -> ((f r11) (Branch p18 p18))
                                        Z  -> let g = \r18 -> case  r18  of 
                                                                    Branch y18 p -> let h = \s18 -> case  s18  of 
                                                                                                            S z9 -> (S (h z9))
                                                                                                            Z  -> (g p)
                                                                                    in h (g y18)
                                                                    Leaf v -> v
                                              in g p18
              in f x (Leaf (S Z)))