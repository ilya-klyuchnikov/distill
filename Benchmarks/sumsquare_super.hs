import Prelude hiding (enumFromTo)

main = (print $ sum (concatMap (\k -> map (\m -> (k * m)) (f'' (1) (k))) (f''' (1))))

f''' = \x -> case (x > ((10000) ::  Int)) of
              True -> []
              False -> (x:f''' ((x + 1)))

f'' = \x k -> case (x > k) of
               True -> []
               False -> (x:f'' ((x + 1)) (k))