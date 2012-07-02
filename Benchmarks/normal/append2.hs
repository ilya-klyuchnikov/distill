module Main(main) where

main = root

root = print (nrev xs)

nrev = \xs -> case xs of 
      [] -> []
      (y:ys) -> app (nrev ys) [y]

app = \xs ys -> case xs of
      [] -> ys
      (z:zs) -> (z:(app zs ys))