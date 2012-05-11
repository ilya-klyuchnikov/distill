import Prelude hiding (zipWith, sum, replicate)

main = (print $ f'' (0) ((*)) (100000) (100000))

f'' = (\(x) (*) (x') (x'') -> (case (x' == 1) of
                                True -> (case (x'' == 1) of
                                          True -> ((1 * 2) + x)
                                          False -> ((1 * 2) + x))
                                False -> (case (x'' == 1) of
                                           True -> (case ((x' - 1) == 1) of
                                                     True -> ((1 * 2) + x)
                                                     False -> ((1 * 2) + x))
                                           False -> f'' (((1 * 2) + x)) ((*)) ((x' - 1)) ((x'' - 1)))))