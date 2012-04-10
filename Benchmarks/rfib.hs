-- !!! the ultra-notorious "nfib 30" does w/ Floats
--
module Main (main) where
import System

nfib :: Double -> Double
nfib n = if n <= 1 then 1 else nfib (n-1) + nfib (n-2) + 1

root x = nfib x

main = print $ root 40
