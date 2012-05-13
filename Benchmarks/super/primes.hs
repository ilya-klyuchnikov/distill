module Main(main) where


import Prelude
       hiding (True, False, succ, map, head, tail, filter, iterate, mod,
               subtract)

 
data Nat = Z
         | S Nat
         deriving Show

 
data List a = Nil
            | Cons a (List a)
            deriving Show

 
data Bool = True
          | False
          deriving Show
x = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
main = (print $ f (x) (f'' (f' (S (S (Z))))))

f = (\(x) (x') -> (case x of
                    Z -> (case x' of
                           [] -> error ("head")
                           (y:ys) -> (case ys of
                                       [] -> y
                                       (v:vs) -> y))
                    S (y) -> f (y) ((case x' of
                                      [] -> []
                                      (y:ys) -> ys))))

f'' = (\(x') -> ((case x' of
                   [] -> error ("head")
                   (o:os) -> (case f''''''' (o) (os) of
                               [] -> error ("head")
                               (y:ys) -> (case ys of
                                           [] -> y
                                           (v:vs) -> y))):f'' ((case x' of
                                                                 [] -> []
                                                                 (o:os) -> f''''' (o) (os)))))

f''''''' = (\(o) (os) -> (case os of
                           [] -> []
                           (y:ys) -> (case f'''''''''''''''' (y) (o) of
                                       Z -> f''''''' (o) (ys)
                                       S (a) -> (y:f''''''' (o) (ys)))))

f'''''''''''''''' = (\(y) (o) -> (case o of
                                   Z -> (case y of
                                          Z -> f'''''''''''''''' (f'''''''''''''''''''''' (y) (o)) (o)
                                          S (x1) -> f'''''''''''''''' (f''''''''''''''''''''' (y) (o)) (o))
                                   S (y1) -> (case y of
                                               Z -> y
                                               S (x1) -> f'''''''''''''''''' (x1) (y1) (y) (o))))

f'''''''''''''''''''''' = (\(y) (o) -> (case o of
                                         Z -> y
                                         S (y1) -> (case y of
                                                     Z -> Z
                                                     S (x1) -> f'''''''''''''''''''''' (x1) (y1))))

f''''''''''''''''''''' = (\(y) (o) -> (case o of
                                        Z -> y
                                        S (y1) -> (case y of
                                                    Z -> Z
                                                    S (x1) -> f''''''''''''''''''''' (x1) (y1))))

f'''''''''''''''''' = (\(x1) (y1) (y) (o) -> (case y1 of
                                               Z -> (case x1 of
                                                      Z -> f'''''''''''''''' (f'''''''''''''''''''' (y) (o)) (o)
                                                      S (x1) -> f'''''''''''''''' (f''''''''''''''''''' (y) (o)) (o))
                                               S (y1) -> (case x1 of
                                                           Z -> y
                                                           S (x1) -> f'''''''''''''''''' (x1) (y1) (y) (o))))

f'''''''''''''''''''' = (\(y) (o) -> (case o of
                                       Z -> y
                                       S (y1) -> (case y of
                                                   Z -> Z
                                                   S (x1) -> f'''''''''''''''''''' (x1) (y1))))

f''''''''''''''''''' = (\(y) (o) -> (case o of
                                      Z -> y
                                      S (y1) -> (case y of
                                                  Z -> Z
                                                  S (x1) -> f''''''''''''''''''' (x1) (y1))))

f''''' = (\(o) (os) -> (case os of
                         [] -> []
                         (y:ys) -> (case f'''''''' (y) (o) of
                                     Z -> f''''' (o) (ys)
                                     S (a) -> (y:f''''' (o) (ys)))))

f'''''''' = (\(y) (o) -> (case o of
                           Z -> (case y of
                                  Z -> f'''''''' (f'''''''''''''' (y) (o)) (o)
                                  S (x1) -> f'''''''' (f''''''''''''' (y) (o)) (o))
                           S (y1) -> (case y of
                                       Z -> y
                                       S (x1) -> f'''''''''' (x1) (y1) (y) (o))))

f'''''''''''''' = (\(y) (o) -> (case o of
                                 Z -> y
                                 S (y1) -> (case y of
                                             Z -> Z
                                             S (x1) -> f'''''''''''''' (x1) (y1))))

f''''''''''''' = (\(y) (o) -> (case o of
                                Z -> y
                                S (y1) -> (case y of
                                            Z -> Z
                                            S (x1) -> f''''''''''''' (x1) (y1))))

f'''''''''' = (\(x1) (y1) (y) (o) -> (case y1 of
                                       Z -> (case x1 of
                                              Z -> f'''''''' (f'''''''''''' (y) (o)) (o)
                                              S (x1) -> f'''''''' (f''''''''''' (y) (o)) (o))
                                       S (y1) -> (case x1 of
                                                   Z -> y
                                                   S (x1) -> f'''''''''' (x1) (y1) (y) (o))))

f'''''''''''' = (\(y) (o) -> (case o of
                               Z -> y
                               S (y1) -> (case y of
                                           Z -> Z
                                           S (x1) -> f'''''''''''' (x1) (y1))))

f''''''''''' = (\(y) (o) -> (case o of
                              Z -> y
                              S (y1) -> (case y of
                                          Z -> Z
                                          S (x1) -> f''''''''''' (x1) (y1))))

f' = (\(x') -> (S (x'):f' (S (x'))))