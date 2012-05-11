module Main(main) where

main = print ((case x of
                0 -> 1
                o -> f' (o - 1)))

f' = (\(o) -> (case o of
                0 -> o + 1
                o' -> (case f' (o' - 1) of
                            0 -> 0
                            z -> f'''' (o) (z - 1))))

f'''' = (\(o) (z) -> (case (case z of
                             0 -> 0
                             z -> f'''' (o) (z - 1)) of
                       0 -> o + 1
                       z -> (f''''' (z - 1) (o)) + 1))

f''''' = (\(z') (o) -> (case z' of
                         0 -> o + 1
                         z -> (f''''' (z - 1) (o)) + 1))

x = 3