import Prelude hiding (map, iterate, filter, head)

main = print ((((f''' (mod) (f'' (2)) !! ((1) ::  Int))) ::  Int))

f''' = (\(mod) (x) -> ((case x of
                         (n:ns) -> (case f'''''''' (mod) (n) (ns) of
                                     [] -> error ("head")
                                     (x:xs) -> x)
                         [] -> (case error ("the_filter") of
                                 [] -> error ("head")
                                 (x:xs) -> x)):f''' (mod) ((case x of
                                                             (n:ns) -> f'''''' (mod) (n) (ns)
                                                             [] -> error ("the_filter")))))

f'''''''' = (\(mod) (n) (ns) -> (case ns of
                                  [] -> []
                                  (x:xs) -> (case (mod (x) (n) /= 0) of
                                              True -> (x:f'''''''' (mod) (n) (xs))
                                              False -> f'''''''' (mod) (n) (xs))))

f'''''' = (\(mod) (n) (ns) -> (case ns of
                                [] -> []
                                (x:xs) -> (case (mod (x) (n) /= 0) of
                                            True -> (x:f'''''' (mod) (n) (xs))
                                            False -> f'''''' (mod) (n) (xs))))

f'' = (\(x) -> ((x + 1):f'' ((x + 1))))