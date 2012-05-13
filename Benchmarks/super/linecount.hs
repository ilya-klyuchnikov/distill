module Main(main) where


import Prelude hiding (head, tail, length)

main = print . f =<< getContents

f = (\(getContents) -> (case getContents of
                         [] -> 0
                         (y:ys) -> (case ys of
                                     [] -> 1
                                     (y:ys) -> (case getContents of
                                                 (y:ys) -> (case y of
                                                             '\n' -> 1 + (f ((case getContents of
                                                                                [] -> []
                                                                                (y:ys) -> ys)))
                                                             _ -> f ((case getContents of
                                                                            [] -> []
                                                                            (y:ys) -> ys)))))))