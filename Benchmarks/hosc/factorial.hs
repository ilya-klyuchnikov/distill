module Main(main) where

data Nat  = Z  | S Nat deriving Show

x = (S (S (S (S (S (S (S (S Z))))))))

main = print (let f = \r3 -> case  r3  of 
                                    Z  -> (S Z)
                                    S v2 -> let g = \s3 -> case  s3  of 
                                                                S s -> let h = \t3 -> case  t3  of 
                                                                                            S p2 -> (S (h p2))
                                                                                            Z  -> (S v2)
                                                                       in h (g s)
                                                                Z  -> Z
                                            in g (f v2) 
              in f x)