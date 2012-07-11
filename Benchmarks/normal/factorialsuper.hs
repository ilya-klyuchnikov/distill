module Main(main) where


 
data Nat = Z
         | S Nat
         deriving Show

main = print root

root = (case x of
         Z -> S (Z)
         S (o) -> (case o of
                    Z -> S (o)
                    S (o') -> f (o') (o)))

f = (\(o') (o) -> (case o' of
                    Z -> f''' (o') (o)
                    S (o'') -> (case f (o'') (o') of
                                 Z -> Z
                                 S (z) -> f' (z) (o))))

f''' = (\(o') (o) -> (case o' of
                       Z -> S (o)
                       S (z) -> (case f''' (o) (z) of
                                  Z -> S (o)
                                  S (z') -> S (f'''' (z') (o)))))

f'''' = (\(z') (o) -> (case z' of
                        Z -> S (o)
                        S (z'') -> S (f'''' (z'') (o))))

f' = (\(z) (o) -> (case z of
                    Z -> S (o)
                    S (z') -> (case f' (o) (z') of
                                Z -> S (o)
                                S (z'') -> S (f'' (z'') (o)))))

f'' = (\(z'') (o) -> (case z'' of
                       Z -> S (o)
                       S (z''') -> S (f'' (z''') (o))))