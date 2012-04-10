main = print $ show (nrev xs)

nrev = \xs->case xs of
               [] -> []
               (x:xs) -> app (nrev xs) [x]
app = \xs ys->case xs of
                 [] -> ys
                 (x:xs) -> (x:app xs ys)