module Main(main) where


import System.Environment (getArgs)

import Arguments

main
  = do args <- getArgs
       let level = read (head args) :: Integer
       print $ root (randomXS level)

root = (\(xs) -> (case xs of
                   [] -> []
                   (y:(ys)) -> f (ys) ((y:[]))))

f = (\(ys) (x) -> (case ys of
                    [] -> x
                    (y':(ys'')) -> f (ys'') ((y':(x)))))