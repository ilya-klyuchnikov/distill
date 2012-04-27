main = (print $ f' (100000))

f' = \(x) -> case (x == 0) of
              True -> 1
              False -> (x * f' ((x - 1)))