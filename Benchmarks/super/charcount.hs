module Main(main) where


main = ((print . (\(xs) -> (case xs of
                             [] -> 0
                             (y:ys) -> (1 + f' (ys))))) =<< getContents)

f' = (\(ys) -> (case ys of
                 [] -> 0
                 (y:ys) -> (1 + f' (ys))))