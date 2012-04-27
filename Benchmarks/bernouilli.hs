
-- There was a lot of discussion about various ways of computing
-- Bernouilli numbers (whatever they are) on haskell-cafe in March 2003
-- Here's one of the programs.

-- It's not a very good test, I suspect, because it manipulates big integers,
-- and so probably spends most of its time in GMP.  

--import Prelude hiding ((!!),map,filter,odd,enumFromTo,error,zipWith,enumFrom,(++),iterate,($),tail,sum,not,head)
--import qualified Prelude
--import Ratio hiding ((%))
--import qualified Ratio
module Main(main) where
import Ratio

-- powers = [[r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction
powers :: [[Integer]]
powers = enumFrom 2 : map (zipWith (*) (head powers)) powers

-- powers = [[(-1)^r * r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction
neg_powers :: [[Integer]]
neg_powers = 
  map (zipWith (\n x -> if n then x else negate x) (iterate not True)) powers

pascal:: [[Integer]]
pascal = iterate op (1:2:1:[])

op line = zipWith (+) (line++[0]) (0:line)


bernoulli :: Int -> Rational
bernoulli n = case n == 0 of
    True -> (1 :: Integer) % (0 :: Integer)
    False -> case n == 1 of
        True -> (1 :: Integer) % (2 :: Integer)
        False -> case odd n of
            True -> 0
            False -> ((1 :: Integer)% (2 :: Integer)) + sum (zipWith (f (neg_powers !! (n - 1))) (enumFromTo 2 n) pascal)

f :: [Integer] -> Int -> [Integer] -> Rational
f powers k combs = ((sum $ zipWith (*) powers (tail $ tail combs)) - 
                           (fromIntegral k :: Integer)) % (fromIntegral (k + 1) :: Integer)
                           
root x = bernoulli x

main = print $ root 1000

