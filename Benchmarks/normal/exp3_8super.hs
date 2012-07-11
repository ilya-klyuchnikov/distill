module Main(main) where


 
data Nat = Z
         | S Nat
         deriving Show

main = print $ root

root = (case x of
         Z -> S (Z)
         S (z) -> f (z))

f = (\(z) -> (case z of
               Z -> S (S (S (Z)))
               S (z') -> (case f (z') of
                           Z -> Z
                           S (z'') -> f' (z''))))

f' = (\(z'') -> (case z'' of
                  Z -> S (S (S (Z)))
                  S (z''') -> (case f' (z''') of
                                Z -> S (S (S (Z)))
                                S (z'''') -> S (f'' (z'''')))))

f'' = (\(z'''') -> (case z'''' of
                     Z -> S (S (S (Z)))
                     S (z''''') -> S (f'' (z'''''))))