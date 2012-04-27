main = print ((((map (head) (iterate (\(ns) -> case ns of
                                                (n:ns) -> filter (\(x) -> (mod (x) (n) /= 0)) (ns)
                                                [] -> error ("the_filter")) (iterate (\(x) -> (x + 1)) (2))) !! ((6000) ::  Int))) ::  Int))