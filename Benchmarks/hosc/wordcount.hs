module Main(main) where

main = print . (\c -> 
  let f = \s8 t8-> case  s8  of
          (s1:r2) -> case  r2  of 
              []  -> 1
              (w4:s2) -> case  t8  of
                  (v1:y3) -> case  v1  of 
                      ' ' -> 1 + f y3 y3
                      _  -> f y3 y3
          []  -> 0
  in f c c) =<< getContents