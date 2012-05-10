main = print . (\c -> g c) =<< getContents 

g :: [Char] -> Int
g x = case  x  of
	(y17:s15) -> case s15 of
				[] -> case y17 of
						'\n' -> 1
						_ -> 1
				(u4:x16) -> f y17 (y17:u4:x16)

f :: Char -> [Char] -> Int
f = \r23 s23 -> case r23 of
		'\n' -> 1 + f (case s23 of
							[] -> ' '
							(p12:x) -> case x of
										"" -> ' '
										(x10:t20) -> case t20 of
														[] -> x10
														(y3:t19) -> x10)
					  (case s23 of
							[] -> []
							(y23:w) -> w)
		_ -> case s23 of
			[] -> 0
			(r20:v2) -> case v2 of
						[] -> 1
						(z17:s6) -> case s6 of
									[] -> f z17 (z17:[])
									(t21:v6) -> f z17 (z17:t21:v6)