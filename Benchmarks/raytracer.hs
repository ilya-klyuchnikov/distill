module Main(main) where
    
import Prelude hiding(zipWith, sum, replicate)

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

replicate n x = case n == 1 of
	True -> [x]
	False -> (x: replicate (n - 1) x)

