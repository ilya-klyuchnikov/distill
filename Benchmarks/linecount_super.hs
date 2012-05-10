import Prelude hiding (length, head, tail)

main = (((print) . (\(xs) -> (case (f' ((xs)) == 1) of
                               True -> 1
                               False -> (case ((case (xs) of
                                                 [] -> (error) ("head")
                                                 (x:xs) -> (x)) == '\n') of
                                          True -> (1 + f''' ((xs)))
                                          False -> f'' ((xs)))))) =<< (getContents))

f''' = (\(xs) -> (case ((case (xs) of
                          [] -> 0
                          (x:xs) -> (case (xs) of
                                      [] -> 0
                                      (x:xs) -> (1 + f''''''' ((xs))))) == 1) of
                   True -> 1
                   False -> (case ((case (xs) of
                                     [] -> (error) ("head")
                                     (x:xs) -> (case (xs) of
                                                 [] -> (error) ("head")
                                                 (x:xs) -> (x))) == '\n') of
                              True -> (1 + f''' ((case (xs) of
                                                   [] -> []
                                                   (x:xs) -> (xs))))
                              False -> f''' ((case (xs) of
                                               [] -> []
                                               (x:xs) -> (xs))))))

f''''''' = (\(xs'') -> (case (xs'') of
                         [] -> 0
                         (x:xs) -> (1 + f''''''' ((xs)))))

f'' = (\(xs) -> (case ((case (xs) of
                         [] -> 0
                         (x:xs) -> (case (xs) of
                                     [] -> 0
                                     (x:xs) -> (1 + f''''' ((xs))))) == 1) of
                  True -> 1
                  False -> (case ((case (xs) of
                                    [] -> (error) ("head")
                                    (x:xs) -> (case (xs) of
                                                [] -> (error) ("head")
                                                (x:xs) -> (x))) == '\n') of
                             True -> (1 + f'' ((case (xs) of
                                                 [] -> []
                                                 (x:xs) -> (xs))))
                             False -> f'' ((case (xs) of
                                             [] -> []
                                             (x:xs) -> (xs))))))

f''''' = (\(xs'') -> (case (xs'') of
                       [] -> 0
                       (x:xs) -> (1 + f''''' ((xs)))))

f' = (\(xs) -> (case (xs) of
                 [] -> 0
                 (x:xs) -> (1 + f' ((xs)))))