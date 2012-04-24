main = show (nrev xs)

nrev = \zs->case zs of
               [] -> []
               (x:xs) -> app (nrev xs) [x]
app = \xs ys->case xs of
                 [] -> ys
                 (x:xs) -> (x:app xs ys)