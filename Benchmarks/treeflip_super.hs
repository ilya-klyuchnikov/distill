import Prelude hiding (flip)

 
data Tree a = Leaf a
            | Branch (Tree a) (Tree a)

main = (print $ f'''' (25) (Leaf 1) (+))

f'''' = \x x' (+) -> case (x == 0) of
                    True -> case x' of
                             Leaf x -> x
                             Branch l r -> (f''''' (l) + f'''''' (r))
                    False -> f'''' ((x - 1)) (Branch x' x') (+)

f'''''' = \r -> case r of
                 Leaf x -> x
                 Branch l r -> (f'''''' (l) + f'''''' (r))

f''''' = \l -> case l of
                Leaf x -> x
                Branch l r -> (f''''' (l) + f''''' (r))