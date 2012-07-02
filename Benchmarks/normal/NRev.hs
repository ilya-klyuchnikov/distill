module Main where
 
import System.Environment (getArgs)
import Arguments
   
main = do
  args <- getArgs
  let level = read (head args) :: Integer
  print $ root (randomXS level)
   
root = \xs -> nrev xs
   
nrev = \xs -> case xs of
  [] -> []
  (y:ys) -> app (nrev ys) [y]
   
app = \xs ys -> case xs of
  [] -> ys
  (z:zs) -> (z:app zs ys)