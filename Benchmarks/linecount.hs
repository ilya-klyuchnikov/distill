import Prelude hiding (length, head, tail)


main = print . linecount =<< getContents

linecount xs
 | length xs == 1 = 1
 | head xs == '\n' = 1 + (linecount (tail xs))
 | otherwise = linecount (tail xs)

head xs = case xs of
	[] -> error "head"
	(x:xs) -> x

tail xs =  case xs of
	[] -> []
	(x:xs) -> xs
	
length xs = case xs of
	[] -> 0
	(x:xs) -> length' 1 xs

length' l xs = case xs of
	[] -> l
	(x:xs) -> length' (l + 1) xs
