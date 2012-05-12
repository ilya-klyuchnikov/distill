module Main(main) where
    
import Prelude hiding (True, False)

data Nat  = Z  | S Nat deriving Show
data List a = Nil  | Cons a (List a) deriving Show
data Bool  = True  | False deriving Show

main = print (let h6 = \x75 y75 -> case  x75  of 
                                            S z60 -> (h6 z60 (case  y75  of
                                                                    Nil  -> Nil
                                                                    Cons p49 p30 -> p30))
                                            Z  -> case  y75  of 
                                                        Cons z26 s36 -> case  s36  of 
                                                                                Nil  -> z26
                                                                                Cons t19 w67 -> z26
              in h6 x (g (f (S (S Z)))))

x = S (S (S (S (S (S Z)))))
 
g = \u71 -> Cons (case  u71  of 
                                                    Cons t70 r51 -> let h = \v71 -> case  v71  of 
                                                                                            Cons v33 u10 -> case (let f1 = \w71 p71 r71 -> case  w71  of 
                                                                                                                                                    S v27 -> case  p71  of 
                                                                                                                                                                    S y71 -> (((f1 v27) y71) r71)
                                                                                                                                                                    Z  -> r71
                                                                                                                                                    Z  -> case  p71  of 
                                                                                                                                                                S p18 -> f1 t70 (let g1 = \s71 t71 -> case  s71  of 
                                                                                                                                                                                                            Z  -> t71
                                                                                                                                                                                                            S t54 -> case  t71  of
                                                                                                                                                                                                                            Z  -> Z
                                                                                                                                                                                                                            S p20 -> ((g1 t54) p20)
                                                                                                                                                                                 in g1 t70 r71) (let h1 = \x72 y72 -> case  x72  of 
                                                                                                                                                                                                                            Z  -> y72
                                                                                                                                                                                                                            S t5 -> case  y72  of 
                                                                                                                                                                                                                                            Z  -> Z
                                                                                                                                                                                                                                            S y41 -> ((h1 t5) y41)
                                                                                                                                                                                                 in h1 t70 r71)
                                                                                                                                                                Z  -> f1 t70 (let f2 = \z72 u72 -> case  z72  of 
                                                                                                                                                                                                            Z  -> u72
                                                                                                                                                                                                            S w68 -> case  u72  of
                                                                                                                                                                                                                            Z  -> Z
                                                                                                                                                                                                                            S w8 -> f2 w68 w8
                                                                                                                                                                              in f2 t70 r71) (let g2 = \v72 w72 -> case  v72  of 
                                                                                                                                                                                                                            Z  -> w72
                                                                                                                                                                                                                            S s63 -> case  w72  of 
                                                                                                                                                                                                                                            Z  -> Z
                                                                                                                                                                                                                                            S r40 -> g2 s63 r40
                                                                                                                                                                                              in g2 t70 r71)
                                                                                                                  in f1 t70 v33 v33) of 
                                                                                                                        Z  -> (h u10)
                                                                                                                        S x35 -> let h2 = \p72 -> case  p72  of 
                                                                                                                                                        Nil  -> v33
                                                                                                                                                        Cons t41 w59 -> case (let f3 = \r72 s72 t72 -> case  r72  of 
                                                                                                                                                                                                            S w1 -> case  s72  of 
                                                                                                                                                                                                                            S z35 -> f3 w1 z35 t72
                                                                                                                                                                                                                            Z  -> t72
                                                                                                                                                                                                            Z  -> case  s72  of 
                                                                                                                                                                                                                        S x55 -> f3 t70 (let g3 = \x73 y73 -> case  x73  of 
                                                                                                                                                                                                                                                                    Z  -> y73
                                                                                                                                                                                                                                                                    S w35 -> case  y73  of 
                                                                                                                                                                                                                                                                                    Z  -> Z
                                                                                                                                                                                                                                                                                    S t1 -> g3 w35 t1 
                                                                                                                                                                                                                                         in g3 t70 t72) (let h3 = \z73 u73 -> case  z73  of 
                                                                                                                                                                                                                                                                                    Z  -> u73
                                                                                                                                                                                                                                                                                    S r24 -> case  u73  of 
                                                                                                                                                                                                                                                                                                    Z  -> Z
                                                                                                                                                                                                                                                                                                    S y6 -> h3 r24 y6
                                                                                                                                                                                                                                                         in h3 t70 t72)
                                                                                                                                                                                                                        Z  -> f3 t70 (let f4 = \v73 w73 -> case  v73  of 
                                                                                                                                                                                                                                                                    Z  -> w73
                                                                                                                                                                                                                                                                    S p17 -> case  w73  of 
                                                                                                                                                                                                                                                                                    Z  -> Z
                                                                                                                                                                                                                                                                                    S z51 -> f4 p17 z51
                                                                                                                                                                                                                                        in f4 t70 t72) (let g4 = \p73 r73 -> case  p73  of 
                                                                                                                                                                                                                                                                                    Z  -> r73
                                                                                                                                                                                                                                                                                    S s47 -> case  r73  of 
                                                                                                                                                                                                                                                                                                    Z  -> Z
                                                                                                                                                                                                                                                                                                    S t10 -> g4 s47 t10
                                                                                                                                                                                                                                                        in g4 t70 t72)
                                                                                                                                                                              in f3 t70 t41 t41) of 
                                                                                                                                                                                  S u1 -> v33
                                                                                                                                                                                  Z  -> (h2 w59)
                                                                                                                                    in h2 u10
                                                                    in h r51) (g (case  u71  of 
                                                                                        Cons p24 y32 -> let h4 = \s73 -> case  s73  of 
                                                                                                                                Nil  -> Nil
                                                                                                                                Cons s70 z54 -> case (let f5 = \t73 x74 y74 -> case  t73  of 
                                                                                                                                                                                    S s11 -> case  x74  of 
                                                                                                                                                                                                    S y16 -> f5 s11 y16 y74
                                                                                                                                                                                                    Z  -> y74
                                                                                                                                                                                    Z  -> case  x74  of 
                                                                                                                                                                                                S s19 -> f5 p24 (let h5 = \v74 w74 -> case  v74  of 
                                                                                                                                                                                                                                            Z  -> w74
                                                                                                                                                                                                                                            S y48 -> case  w74  of 
                                                                                                                                                                                                                                                            Z  -> Z
                                                                                                                                                                                                                                                            S y10 -> h5 y48 y10
                                                                                                                                                                                                                 in h5 p24 y74) (let g5 = \z74 u74 -> case  z74  of 
                                                                                                                                                                                                                                                            Z  -> u74
                                                                                                                                                                                                                                                            S u15 -> case  u74  of 
                                                                                                                                                                                                                                                                            Z  -> Z
                                                                                                                                                                                                                                                                            S s39 -> g5 u15 s39
                                                                                                                                                                                                                                 in g5 p24 y74)
                                                                                                                                                                                                Z  -> f5 p24 (let g6 = \s74 t74 -> case  s74  of 
                                                                                                                                                                                                                                            Z  -> t74
                                                                                                                                                                                                                                            S u7 -> case  t74  of 
                                                                                                                                                                                                                                                            Z  -> Z
                                                                                                                                                                                                                                                            S r43 -> g6 u7 r43
                                                                                                                                                                                                              in g6 p24 y74) (let f6 = \p74 r74 -> case  p74  of 
                                                                                                                                                                                                                                                            Z  -> r74
                                                                                                                                                                                                                                                            S u30 -> case  r74  of 
                                                                                                                                                                                                                                                                            Z  -> Z
                                                                                                                                                                                                                                                                            S u11 -> f6 u30 u11
                                                                                                                                                                                                                              in f6 p24 y74) 
                                                                                                                                                      in f5 p24 s70 s70) of 
                                                                                                                                                          S t42 -> (Cons s70 (h4 z54))
                                                                                                                                                          Z  -> (h4 z54)
                                                                                                        in h4 y32))
f =  \z71 -> (Cons (S z71) (f (S z71))) 