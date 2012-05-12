module Main(main) where

data Nat  = Z  | S Nat deriving Show

x = S (S (S (S (S (S (S (S (S Z))))))))

main = let f = \z4 -> case  z4  of 
                            Z  -> (S Z)
                            S t2 -> let g = \u4 -> case  u4  of
                                                        S y -> let h = \v4 -> case  v4  of 
                                                                                    S u -> (S (h u))
                                                                                    Z  -> (S (S (S Z))) 
                                                               in (h (g y))
                                                        Z  -> Z
                                    in g (f t2)
       in print (f x)