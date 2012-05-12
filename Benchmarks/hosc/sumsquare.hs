module Main(main) where
    
import Prelude hiding (True, False)
    
data Nat  = Z  | S Nat deriving Show
data List a = Nil  | Cons a (List a) deriving Show
data Bool  = True  | False deriving Show

x = S (S Z)

main = print $ case (case  x  of 
                          Z  -> Nil;
                          S x13 -> case  x13  of 
                              Z  -> let g = \w112 p112 -> case  w112  of 
                                                                False  -> Cons p112 (g (case  p112  of
                                                                                                Z  -> False
                                                                                                S p79 -> case  p79  of 
                                                                                                                Z  -> True 
                                                                                                                S p29 -> True) (let h = \r112 -> case  r112  of
                                                                                                                                                        Z  -> S Z
                                                                                                                                                        S p46 -> S (h p46) 
                                                                                                                                 in h p112))
                                                                True  -> Nil
                                    in g False (S Z)
                              S t36 -> let f1 = \s112 t112-> case  s112  of 
                                                                      False  -> Cons t112 (f1 (case  t112  of
                                                                                                        Z  -> False
                                                                                                        S w87 -> case  w87  of 
                                                                                                                        Z  -> case  t36  of 
                                                                                                                                        Z  -> False
                                                                                                                                        S p19 -> False
                                                                                                                        S w56 -> let h1 = \y113 z113 -> case  y113  of
                                                                                                                                                                Z  -> case  z113  of 
                                                                                                                                                                            Z  -> True
                                                                                                                                                                            S t47 -> case  t47  of 
                                                                                                                                                                                            Z  -> False
                                                                                                                                                                                            S v61 -> False
                                                                                                                                                                S s80 -> case  z113  of 
                                                                                                                                                                                Z  -> True
                                                                                                                                                                                S u60 -> h1 s80 u60
                                                                                                                                 in h1 w56 t36) (let g1 = \x113 -> case  x113  of 
                                                                                                                                                                            Z  -> S Z
                                                                                                                                                                            S y74 -> S (g1 y74) in g1 t112))
                                                                      True  -> Nil
                                       in f1 False (S Z)) of
                                           Nil  -> Z
                                           Cons s32 v99 -> let h3 = \y114 z114 u114 v114 -> case  y114  of
                                                                                                    Cons p9 w19 -> h3 w19 z114 u114 (let h4 = \r114 -> case  r114  of
                                                                                                                                                                S s59 -> S (h4 s59)
                                                                                                                                                                Z  -> v114
                                                                                                                                     in h4 (let f4 = \w114 -> case  w114  of
                                                                                                                                                                        Z  -> Z
                                                                                                                                                                        S p105 -> let g4= \p114 -> case  p114  of
                                                                                                                                                                                                            S v43 -> S (g4 v43) 
                                                                                                                                                                                                            Z  -> z114
                                                                                                                                                                                  in g4 (f4 p105)
                                                                                                                                            in f4 p9))
                                                                                                    Nil  -> case  u114  of 
                                                                                                                    Nil  -> v114
                                                                                                                    Cons y110 u97 -> h3 (case  y110  of
                                                                                                                                                Z  -> Nil
                                                                                                                                                S s70 -> case  s70  of
                                                                                                                                                                Z  -> let f5 = \s114 t114 -> case  s114  of
                                                                                                                                                                                                    True  -> Nil
                                                                                                                                                                                                    False  -> Cons t114 (f5 (case  t114  of 
                                                                                                                                                                                                                                    Z  -> False
                                                                                                                                                                                                                                    S y109 -> case  y109  of 
                                                                                                                                                                                                                                                        Z  -> True
                                                                                                                                                                                                                                                        S p96 -> True) (let g5 = \x115 -> case  x115  of 
                                                                                                                                                                                                                                                                                                    Z  -> (S Z)
                                                                                                                                                                                                                                                                                                    S y89 -> (S (g5 y89))
                                                                                                                                                                                                                                                                        in g5 t114))
                                                                                                                                                                      in f5 False (S Z)
                                                                                                                                                                S x18 -> let h5 = \y115 z115-> case  y115  of 
                                                                                                                                                                                                        False  -> Cons z115 (h5 (case  z115  of 
                                                                                                                                                                                                                                        Z  -> False
                                                                                                                                                                                                                                        S t31 -> case  t31  of 
                                                                                                                                                                                                                                                        Z  -> case  x18  of 
                                                                                                                                                                                                                                                                    Z  -> False
                                                                                                                                                                                                                                                                    S p63 -> False
                                                                                                                                                                                                                                                        S u78 -> let g6 = \v115 w115 -> case  v115  of 
                                                                                                                                                                                                                                                                                                Z  -> case  w115  of
                                                                                                                                                                                                                                                                                                                Z  -> True
                                                                                                                                                                                                                                                                                                                S r95 -> case  r95  of 
                                                                                                                                                                                                                                                                                                                                Z  -> False
                                                                                                                                                                                                                                                                                                                                S r5 -> False
                                                                                                                                                                                                                                                                                                S x106 -> case  w115  of 
                                                                                                                                                                                                                                                                                                                Z  -> True
                                                                                                                                                                                                                                                                                                                S u57 -> g6 x106 u57
                                                                                                                                                                                                                                                                 in g6 u78 x18) (let f6 = \u115 -> case  u115  of 
                                                                                                                                                                                                                                                                                                            Z  -> (S Z)
                                                                                                                                                                                                                                                                                                            S w32 -> (S (f6 w32))
                                                                                                                                                                                                                                                                                 in f6 z115))
                                                                                                                                                                                                        True  -> Nil
                                                                                                                                                                         in h5 False (S Z)) y110 u97 v114
                                                           in h3 (case  s32  of 
                                                                        Z  -> Nil
                                                                        S z13 -> case  z13  of 
                                                                                        Z  -> let f2 = \u113 v113 -> case  u113  of 
                                                                                                                            False  -> Cons v113 (f2 (case  v113  of 
                                                                                                                                                            Z  -> False
                                                                                                                                                            S v56 -> case  v56  of 
                                                                                                                                                                            Z  -> True
                                                                                                                                                                            S y104 -> True) (let g2 = \w113 -> case  w113  of 
                                                                                                                                                                                                                        Z  -> (S Z)
                                                                                                                                                                                                                        S u101 -> (S (g2 u101))
                                                                                                                                                                                             in g2 v113))
                                                                                                                            True  -> Nil
                                                                                              in f2 False (S Z)
                                                                                        S y32 -> let h2 = \p113 r113 -> case  p113  of 
                                                                                                                                False  -> Cons r113 (h2 (case  r113  of 
                                                                                                                                                                Z  -> False
                                                                                                                                                                S w92 -> case  w92  of 
                                                                                                                                                                                Z  -> case  y32  of 
                                                                                                                                                                                                Z  -> False
                                                                                                                                                                                                S w77 -> False
                                                                                                                                                                                S u15 -> let g3 = \t113 x114 -> case  t113  of
                                                                                                                                                                                                                        Z  -> case  x114  of
                                                                                                                                                                                                                                    Z  -> True
                                                                                                                                                                                                                                    S t73 -> case  t73  of 
                                                                                                                                                                                                                                                    Z  -> False
                                                                                                                                                                                                                                                    S y66 -> False
                                                                                                                                                                                                                        S v29 -> case  x114  of 
                                                                                                                                                                                                                                        Z  -> True
                                                                                                                                                                                                                                        S w27 -> g3 v29 w27
                                                                                                                                                                                         in g3 u15 y32) (let f3 = \s113 -> case  s113  of 
                                                                                                                                                                                                                                 Z  -> (S Z)
                                                                                                                                                                                                                                 S t105 -> (S (f3 t105))
                                                                                                                                                                                                         in f3 r113))
                                                                                                                                True  -> Nil
                                                                                                 in h2 False (S Z)) s32 v99 Z
                                          