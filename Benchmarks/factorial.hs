module Main(main, fac) where
    
fac :: Integer -> Integer
fac n = case n == 0 of
    True -> 1
    False -> n * (fac (n-1)::Integer)

root x = fac x

main = print $ (root (100000::Integer) :: Integer)
