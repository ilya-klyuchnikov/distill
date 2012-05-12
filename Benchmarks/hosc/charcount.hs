module Main(main) where

main = print . (\c -> (let f = \x1 -> case  x1  of 
                                            []  -> 0
                                            (z:v) -> 1 + f v 
                       in f c))  =<< getContents