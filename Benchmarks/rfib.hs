module Main (main) where

nfib n = if n <= 1 then (1 :: Double) else nfib (n-1) + nfib (n-2) + 1

root x = nfib x

main = print $ root 40
