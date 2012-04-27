import Foreign.C.Types

import System.IO

import System.IO.Unsafe

main = ((print . \x -> case (head (x) == ' ') of
                        True -> (1 + f'' (tail) (x))
                        False -> case (length (x) == 1) of
                                  True -> 1
                                  False -> f' (tail) (x)) =<< getContents)

f'' = \tail x -> case (head (tail (x)) == ' ') of
                  True -> (1 + f'' (tail) (tail (x)))
                  False -> case (length (tail (x)) == 1) of
                            True -> 1
                            False -> f'' (tail) (tail (x))

f' = \tail x -> case (head (tail (x)) == ' ') of
                 True -> (1 + f' (tail) (tail (x)))
                 False -> case (length (tail (x)) == 1) of
                           True -> 1
                           False -> f' (tail) (tail (x))