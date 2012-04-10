
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
powers = enumFrom integer2'0 : map (zipWith (mulInteger'2) (head powers)) powers

-- powers = [[(-1)^r * r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction
neg_powers :: [[Integer]]
neg_powers = 
  map (zipWith (\n x -> if n then x else negateInteger'1 x) (iterate not True)) powers

pascal:: [[Integer]]
pascal = iterate op (integer1'0:integer2'0:integer1'0:[])
op line = zipWith (addInteger'2) (line++[integer0'0]) (integer0'0:line)


bernoulli :: Int -> Rational
bernoulli n = case eqInt'2 n int0'0 of
    True -> integer1'0%integer1'0
    False -> case eqInt'2 n int1'0 of
        True -> integer_1'0%integer2'0
        False -> case odd n of
            True -> 0
            False -> let ps = neg_powers !! (subInt'2 n int1'0)
                     in (integer_1'0%integer2'0) `addRational'2` sumRational (zipWith (f ps) (enumFromTo int2'0 n) pascal)

f :: [Integer] -> Int -> [Integer] -> Rational
f powers k combs = ((sumInteger $ zipWith (mulInteger'2) powers (tail $ tail combs)) `subInteger'2` 
                            intToInteger'1 k) % intToInteger'1 (addInt'2 k int1'0)


root x = bernoulli x

intToInteger'1 = Prelude.fromIntegral :: Int -> Integer
eqInt'2 = (Prelude.==) :: Int -> Int -> Bool
mulInteger'2 = (Prelude.*) :: Integer -> Integer -> Integer
negateInteger'1 = Prelude.negate :: Integer -> Integer
addInteger'2 = (Prelude.+) :: Integer -> Integer -> Integer
subInt'2 = (Prelude.-) :: Int -> Int -> Int
addInt'2 = (Prelude.+) :: Int -> Int -> Int
modInt'2 = Prelude.mod :: Int -> Int -> Int
addRational'2 = (Prelude.+) :: Rational -> Rational -> Rational
gtInt'2 = (Prelude.>) :: Int -> Int -> Bool
subInteger'2 = (Prelude.-) :: Integer -> Integer -> Integer
rational'2 = (Ratio.%) :: Integer -> Integer -> Rational
error'1 = Prelude.error
sumRational = Prelude.sum :: [Rational] -> Rational
sumInteger = Prelude.sum :: [Integer] -> Integer

int0'0 = 0 :: Int
int1'0 = 1 :: Int
int2'0 = 2 :: Int
integer0'0 = 0 :: Integer
integer1'0 = 1 :: Integer
integer2'0 = 2 :: Integer
integer_1'0 = -1 :: Integer
rational0'0 = 0 :: Rational

main = print $ root (1000 :: Int)

