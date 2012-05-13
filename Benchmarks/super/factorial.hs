module Main(main) where

data Nat = Z
         | S Nat
         deriving Show

main = print (f (x))

f = (\(x) -> (case x of
               Z -> S (Z)
               S (o) -> (case f (o) of
                          Z -> Z
                          S (z) -> f' (o) (z) (o))))

f' = (\(x'') (z) (x''') -> (case (case z of
                                   Z -> Z
                                   S (z) -> f' (x'') (z) (x'')) of
                             Z -> S (x''')
                             S (z) -> S (f'' (z) (x'''))))

f'' = (\(z') (x''') -> (case z' of
                         Z -> S (x''')
                         S (z) -> S (f'' (z) (x'''))))

x = (S (S (S (S (S (S (S (S Z))))))))