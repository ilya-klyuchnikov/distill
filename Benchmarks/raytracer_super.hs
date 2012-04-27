import Prelude hiding (zipWith, sum)

main = (print $ case replicate (100000) (1) of
                 [] -> 0
                 (x:xs) -> case replicate (100000) (2) of
                            [] -> 0
                            (y:ys) -> f'''' (+) (*) (x) (y) (0) (xs) (ys))

f'''' = \(+) (*) (x) (y) (x') (xs) (ys) -> case xs of
                                            [] -> ((x * y) + x')
                                            (x'':xs) -> case ys of
                                                         [] -> ((x * y) + x')
                                                         (y':ys) -> f'''' (+) (*) (x'') (y') (((x * y) + x')) (xs) (ys)