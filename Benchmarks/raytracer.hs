module Main(main) where
    
import Prelude hiding(zipWith, sum)

zipWith f xs ys = case xs of
    [] -> []
    (x:xs) -> case ys of
        [] -> []
        (y:ys) -> (f x y : zipWith f xs ys)

sum xs = sumWith 0 xs

sumWith acc xs = case xs of
    [] -> acc
    (x:xs) -> sumWith (x+acc) xs

root xs ys = sum (zipWith (*) xs ys)

main = let n = 100000
       in print $ root ((replicate n 1)) ((replicate n 2))

