module Main(main) where

main = print . charcount =<< getContents

charcount = \xs -> case xs of
      [] -> 0
      (y:ys) -> 1 + (charcount ys)