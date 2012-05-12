module Main(main) where

import Prelude hiding (True, False)

data Nat  = Z  | S Nat deriving Show
data Bool  = True  | False deriving Show

x = (S (S (S Z)))

main = print (case  x  of 
                    Z  -> (S Z)
                    S v1 -> case  v1  of 
                                    Z  -> (S Z)
                                    S x7 -> let f = \u24 v24 -> case  u24  of 
                                                                        True  -> (S Z)
                                                                        False  -> let g = \w24 -> case  w24  of
                                                                                                        S y12 -> (S (g y12))
                                                                                                        Z  -> let h = \p24 -> case  p24  of 
                                                                                                                                    S r11 -> (S (h r11))
                                                                                                                                    Z  -> (S Z)
                                                                                                              in h (f (case  v24  of 
                                                                                                                                Z  -> True
                                                                                                                                S s21 -> case  s21  of 
                                                                                                                                                Z  -> True
                                                                                                                                                S r16 -> case  r16  of 
                                                                                                                                                                Z  -> True
                                                                                                                                                                S w11 -> case  w11  of 
                                                                                                                                                                                Z  -> True
                                                                                                                                                                                S v19 -> False) (case  v24  of 
                                                                                                                                                                                                        Z  -> Z
                                                                                                                                                                                                        S t13 -> case  t13  of 
                                                                                                                                                                                                                        Z  -> Z
                                                                                                                                                                                                                        S v2 -> v2))
                                                                                  in g (f (case  v24  of 
                                                                                                    Z  -> True
                                                                                                    S v13 -> case  v13  of 
                                                                                                                    Z  -> True
                                                                                                                    S p4 -> case  p4  of 
                                                                                                                                    Z  -> True
                                                                                                                                    S s19 -> False) (case  v24  of 
                                                                                                                                                            Z  -> Z
                                                                                                                                                            S x24 -> x24))
                                            in f False (S (S x7)))