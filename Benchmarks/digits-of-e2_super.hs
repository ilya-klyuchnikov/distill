main = (print $ take (10000) (++ ("2") ((((((tail . concat) $ map ((show . head))) $ iterate (((\(ds) -> case ds of
                                                                                                          (d:ds) -> case div ((div (d) (2) == (d + 9))) (2) of
                                                                                                                     True -> ((div (d) (2):(mod (d) (2) + head (nextcarry_fraction))):tail (nextcarry_fraction))
                                                                                                                     False -> ((div ((d + nextcarry)) (2):mod ((d + nextcarry)) (2)):tail (nextcarry_fraction))
                                                                                                          [] -> error ("carryPropagate") . map (\(x) -> (10 * x))) . tail))) $ 2):repeat (1)))))