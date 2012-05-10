main = let
    f= \t9 x10-> case  t9  of
           (u8:v2) -> case  v2  of
              (v1:t) -> case  t  of
                  []  -> 1 + ((f (case  x10  of 
								[] -> []
								(r2:x4) -> x4)
                        	   (case  x10  of
  								[] -> [] 
								(u6:v6) -> v6)))
                  (v9:s5) -> 1 + ((f (case  x10  of 
										[] -> []
										(u1:r7) -> r7)
                        			 (case  x10  of
 										[] -> []
										(u:y1) -> y1)))
              []  -> 1
  in
    (print . (\c -> f c c) =<< getContents)