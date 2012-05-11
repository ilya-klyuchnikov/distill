import Prelude hiding (enumFromTo, sum, map)

main = (print $ (case concatMap ((\(k) -> f'''' ((*)) (k) (1))) (f''''' (1)) of
                  [] -> 0
                  (x:xs) -> f'''''' ((+)) (x) (0) (xs)))

f'''''' = (\(+) (x) (x') (xs) -> (case xs of
                                   [] -> (x + x')
                                   (x'':xs) -> f'''''' ((+)) (x'') ((x + x')) (xs)))

f''''' = (\(x) -> (case (x > ((10000) ::  Int)) of
                    True -> []
                    False -> (x:f''''' ((x + 1)))))

f'''' = (\(*) (k) (x) -> (case (x > k) of
                           True -> []
                           False -> ((k * x):f'''' ((*)) (k) ((x + 1)))))