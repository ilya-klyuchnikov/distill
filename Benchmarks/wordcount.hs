import Prelude hiding (length, head, tail)

main = print . wordCount =<< getContents

wordCount x
 | length x == 1 = 1
 | head x == ' ' = 1 + (wordCount (tail x))
 | otherwise = wordCount (tail x)

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
