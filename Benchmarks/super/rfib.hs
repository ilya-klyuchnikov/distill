main = (print $ f' (40))

f' = (\(x) -> (case (x <= 1) of
                True -> ((1) ::  Double)
                False -> ((f' ((x - 1)) + f' ((x - 2))) + 1)))