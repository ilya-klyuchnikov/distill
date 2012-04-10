module Main(main) where
import Prelude hiding (flip)

data Tree a = Leaf a | Branch (Tree a) (Tree a)

flip t = case t of
    (Leaf x) -> Leaf x
    (Branch l r) -> Branch (flip l) (flip r)

sumtr t = case t of
    Leaf x -> x
    Branch l r -> sumtr l + sumtr r

buildTree n t = case n == 0 of
    True -> t
    False -> buildTree (n-1) (Branch t t)

root :: Int -> Int
root n = sumtr (flip (flip (buildTree n (Leaf 1))))

main = print $ root 25




