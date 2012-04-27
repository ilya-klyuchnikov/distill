import System

main = (print $ f' (40))

f' :: Double -> Double
f' = \x -> case (x <= 1) of
            True -> 1
            False -> ((f' ((x - 1))) + f' ((x - 2))) + 1