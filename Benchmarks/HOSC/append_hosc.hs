main = print $ show $
    let
      f = \y3 -> case  y3  of
          	[]  -> []
          	(p1:w1) -> let g = \z3 -> case  z3  of 
										(w2:r1) -> (w2:g r1)
										[] -> (p1:[])
					   in (g (f w1))
	  in f xs
	
xs = [1..10000]