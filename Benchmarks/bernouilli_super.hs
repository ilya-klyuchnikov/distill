import Ratio

main = (print $ case (1000 == 0) of
                 True -> (1 % 0)
                 False -> case (1000 == 1) of
                           True -> (1 % 2)
                           False -> case odd (1000) of
                                     True -> 0
                                     False -> ((1 % 2) + sum (zipWith (\(k) (combs) -> (((sum $ zipWith (*) ((map (zipWith (\(n) (x) -> case n of
                                                                                                                                         True -> x
                                                                                                                                         False -> negate (x)) (iterate (not) (True))) (f'''') !! (1000 - 1))) ((tail $ tail (combs)))) - fromIntegral (k)) % fromIntegral ((k + 1)))) (enumFromTo (2) (1000)) (iterate (\(line) -> (zipWith (+) (line ++ [0]) ((0:line)))) ([1,2,1])))))

f'''' = (enumFrom (2):map (zipWith (*) (head (f''''))) (f''''))