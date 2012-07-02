module Main(main) where

main = root

root = print (f (xs))

f = (\(xs) -> (case xs of
                [] -> []
                (y:(ys)) -> (case f (ys) of
                              [] -> (y:[])
                              (z:(zs)) -> (z:(f'' (zs) (y))))))

f'' = (\(zs) (y) -> (case zs of
                      [] -> (y:[])
                      (z:(zs)) -> (z:(f'' (zs) (y)))))

xs = [1..1000]