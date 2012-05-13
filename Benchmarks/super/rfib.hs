module Main(main) where


import Prelude hiding (subtract, True, False)

 
data Nat = Z
         | S Nat
         deriving Show

 
data Bool = True
          | False
          deriving Show

x = fromInt 35

fromInt x = if x < 1 then Z else S (fromInt (x-1))

main = print (f (x))

f = (\(x) -> (case x of
               Z -> S (Z)
               S (x1) -> (case x1 of
                           Z -> S (Z)
                           S (x1) -> (case f ((case x of
                                                Z -> Z
                                                S (x1) -> x1)) of
                                       Z -> (case f ((case x of
                                                       Z -> Z
                                                       S (x1) -> (case x1 of
                                                                   Z -> Z
                                                                   S (x1) -> x1))) of
                                              Z -> S (Z)
                                              S (z) -> S (f'''''' (z)))
                                       S (z) -> S (f''''' (z) (x))))))

f'''''' = (\(z) -> (case z of
                     Z -> S (Z)
                     S (z) -> S (f'''''' (z))))

f''''' = (\(z) (x) -> (case z of
                        Z -> (case f ((case x of
                                        Z -> Z
                                        S (x1) -> (case x1 of
                                                    Z -> Z
                                                    S (x1) -> x1))) of
                               Z -> S (Z)
                               S (z) -> S (f''''''' (z)))
                        S (z) -> S (f''''' (z) (x))))

f''''''' = (\(z') -> (case z' of
                       Z -> S (Z)
                       S (z) -> S (f''''''' (z))))