 
data Nat = Z
         | S Nat

main = (print $ (((case f''' (((9) ::  Int)) of
                    Z -> 0
                    S (x) -> (1 + f'''' (x)))) ::  Int))

f'''' = (\(x') -> (case x' of
                    Z -> 0
                    S (x) -> (1 + f'''' (x))))

f''' = (\(x) -> (case (x < 1) of
                  True -> S (Z)
                  False -> (case f''' ((x - 1)) of
                             Z -> Z
                             S (y) -> f''''''' (y))))

f''''''' = (\(y) -> (case (case y of
                            Z -> Z
                            S (y) -> f''''''' (y)) of
                      Z -> f'''''''''' (3)
                      S (x) -> S (f'''''''' (x))))

f'''''''''' = (\(x''') -> (case (x''' < 1) of
                            True -> Z
                            False -> S (f'''''''''' ((x''' - 1)))))

f'''''''' = (\(x''') -> (case x''' of
                          Z -> f''''''''' (3)
                          S (x) -> S (f'''''''' (x))))

f''''''''' = (\(x'''') -> (case (x'''' < 1) of
                            True -> Z
                            False -> S (f''''''''' ((x'''' - 1)))))