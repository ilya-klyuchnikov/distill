module Main(main) where
	
import Prelude hiding (map, iterate, filter, head)

suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter ns = case ns of
    (n:ns) -> filter (isdivs n) ns
    [] -> error "the_filter"

primes :: [Int]
primes = map head (iterate the_filter (iterate suCC 2))

root x = primes !! x

main = print (root (60000 :: Int) :: Int)

map f xs = case xs of
	[] -> []
	(x:xs) -> (f x:map f xs)

filter f xs = case xs of
	[] -> []
	(x:xs) -> case f x of
		True -> (x:filter f xs)
		False -> filter f xs

iterate f x = (f x:iterate f (f x))

head xs = case xs of
	[] -> error "head"
	(x:xs) -> x