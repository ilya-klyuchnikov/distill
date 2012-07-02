module Main(main) where


import System.Environment (getArgs)

import Arguments

main
  = do args <- getArgs
       let level = read (head args) :: Integer
       print $ root (randomXS level)

root = (\(xs) -> f (xs))

f = (\(xs) -> (case xs of
                [] -> []
                (y:(ys)) -> (case f (ys) of
                              [] -> (y:[])
                              (z:(zs)) -> (z:(f'' (zs) (y))))))

f'' = (\(zs) (y) -> (case zs of
                      [] -> (y:[])
                      (z:(zs)) -> (z:(f'' (zs) (y)))))