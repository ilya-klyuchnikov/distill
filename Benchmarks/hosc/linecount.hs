module Main(main) where

main = print . (\c -> (let f = \s8 t8 -> case  s8  of 
                                            [] -> 0
                                            (v2:y1) -> case  y1  of
                                                []-> 1
                                                (p7:y5) -> case  t8  of 
                                                    (y8:y4) ->case  y8  of
                                                        '\n' -> 1 + f y4 y4
                                                        _ -> f y4 y4
                       in f c c)) =<< getContents