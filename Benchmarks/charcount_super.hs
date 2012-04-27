import Foreign.C.Types

import System.IO

import System.IO.Unsafe

main = ((print . \xs -> case (length (xs) == 1) of
                         True -> 1
                         False -> (1 + f' (tail) (xs))) =<< getContents)

f' = \tail xs -> case (length (tail (xs)) == 1) of
                  True -> 1
                  False -> (1 + f' (tail) (tail (xs)))