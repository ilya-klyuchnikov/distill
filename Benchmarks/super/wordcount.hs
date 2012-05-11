import Prelude hiding (length, head, tail)

main = ((print . (\(x) -> (case ((case x of
                                   [] -> 0
                                   (x:xs) -> f'' (1) (xs)) == 1) of
                            True -> 1
                            False -> (case ((case x of
                                              [] -> error ("head")
                                              (x:xs) -> x) == ' ') of
                                       True -> (1 + f''' (x))
                                       False -> f' (x))))) =<< getContents)

f''' = (\(x) -> (case ((case x of
                         [] -> 0
                         (x:xs) -> (case xs of
                                     [] -> 0
                                     (x:xs) -> f''''''' (1) (xs))) == 1) of
                  True -> 1
                  False -> (case ((case x of
                                    [] -> error ("head")
                                    (x:xs) -> (case xs of
                                                [] -> error ("head")
                                                (x:xs) -> x)) == ' ') of
                             True -> (1 + f''' ((case x of
                                                  [] -> []
                                                  (x:xs) -> xs)))
                             False -> f''' ((case x of
                                              [] -> []
                                              (x:xs) -> xs)))))

f''''''' = (\(x''') (xs') -> (case xs' of
                               [] -> x'''
                               (x:xs) -> f''''''' ((x''' + 1)) (xs)))

f' = (\(x) -> (case ((case x of
                       [] -> 0
                       (x:xs) -> (case xs of
                                   [] -> 0
                                   (x:xs) -> f''''' (1) (xs))) == 1) of
                True -> 1
                False -> (case ((case x of
                                  [] -> error ("head")
                                  (x:xs) -> (case xs of
                                              [] -> error ("head")
                                              (x:xs) -> x)) == ' ') of
                           True -> (1 + f' ((case x of
                                              [] -> []
                                              (x:xs) -> xs)))
                           False -> f' ((case x of
                                          [] -> []
                                          (x:xs) -> xs)))))

f''''' = (\(x''') (xs') -> (case xs' of
                             [] -> x'''
                             (x:xs) -> f''''' ((x''' + 1)) (xs)))

f'' = (\(x'') (xs) -> (case xs of
                        [] -> x''
                        (x:xs) -> f'' ((x'' + 1)) (xs)))