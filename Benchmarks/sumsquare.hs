module Main(main) where

import Prelude hiding(enumFromTo, sum, map)

f :: Int -> Int
-- f n = sum [ k * m | k <- [1..n], m <- [1..k] ]
f n = sum (concatMap (\k -> map (\m -> k * m) (enumFromTo 1 k)) (enumFromTo 1 n))

root x = f x

main = print $ root (10000 :: Int)

enumFromTo i j = if i > j then [] else i : enumFromTo (i+1) j

map f x = case x of
    y:ys -> f y : map f ys
    [] -> []

sum xs = sumWith 0 xs

sumWith acc xs = case xs of
    [] -> acc
    x:xs -> sumWith (x+acc) xs