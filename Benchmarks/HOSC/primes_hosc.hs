main = print $ 
	let 
		g = \z63 -> case  z63  of 
						[]  -> []
						(p36:w52) ->
							let h = \u63 -> case  u63  of
												(v36:w49) -> 
													let f1 = \v63 w63 p63-> case  v63  of 
																				0 -> case  w63  of
																						0  -> v36
																						_ -> let
																								g1 = \r63 s63 -> case  r63  of
																													0  -> s63
																													i -> case  s63  of 
																															0 -> 0
																															j -> g1 (i - 1) (j - 1)
																								h1 = \t63 x64 -> case  t63  of
																													0  -> x64
																													i -> case  x64  of
																															0  -> 0
																															j -> h1 (i - 1) (j - 1)
																							 in f1 v36 (g1 v36 p63) (h1 v36 p63)
																				i -> case  w63  of 
																						0 -> v36
																						j -> f1 (i - 1) (j - 1) p63
													in case (f1 v36 p36 p36) of
														0  -> 
															let f2 = \y64 -> case  y64  of
																				[]  -> v36
																				(s58:p48) -> 
																					let g2 = \z64 u64 v64 -> case  z64  of
																												0  -> case  u64  of
																														0 -> s58
																														_ -> let 
																																f3 = \r64 s64 -> case  r64  of
																																					0  -> s64
																																					x50 -> case  s64  of 
																																							0  -> 0 
																																							u39 -> f3 (x50 - 1) (u39 - 1)																					
																																h2 = \w64 p64 -> case  w64  of
																																					0  -> p64
																																					i -> case  p64  of 
																																							0  -> 0
																																							j -> h2 (i - 1) (j - 1)
																															 in g2 s58 (f3 s58 v64) (h2 s58 v64)
																												i -> case  u64  of
																														0  -> s58
																														j -> g2 (i - 1) (j - 1) v64
																					in case g2 s58 p36 p36 of
																						0 -> v36
																						_ -> f2 p48
															in f2 w49
														_ -> h w49
							in ((h w52):(g (case  z63  of
													[] -> []
													(w21:w7) -> 
														let g3= \t64 -> case  t64  of
																			[]  -> []
																			(w42:p59) ->
																				let h3 = \x65 y65 z65 -> case  x65  of
																											0  -> case  y65  of
																													0 -> w42
																													u28 -> case  y65  of 
																															0 -> w42
																															r28 -> h3 (u28 - 1) (r28 - 1) z65
																											_ -> 
																												let 
																													f4 = \u65 v65 -> case  u65  of
																																		0  -> v65
																																		t28 -> case  v65  of 
																																				0 -> 0 
																																				x63 -> f4 (t28 - 1) (x63 - 1)
																													g4 = \w65 p65 -> case  w65  of
																																		0  -> p65
																																		i -> case  p65  of 
																																				0 -> 0
										 																										j -> g4 (i - 1) (j - 1)
																												in h3 w42 (f4 w42 z65) (g4 w42 z65)
																				in case h3 w42 w21 w21 of
																					0  -> (w42:g3 p59)
																					_ -> g3 p59
														in g3 w7)))
	in (g (let f= \y63 -> ((y63 + 1):(f (y63 + 1))) in (f 2)) !! x)
	
x = (1 :: Int)