import Debug.Trace

main = (print $ show f xs)

f = \xs -> case xs of
            [] -> []
            (x:xs) -> case f xs of
                       [] -> [x]
                       (x':xs) -> (x':f'' xs x)

f'' = \xs'' x -> case xs'' of
                  [] -> [x]
                  (x':xs) -> (x':f'' xs x)