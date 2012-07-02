module Main(main) where


main = root

root = print ((case xs of
                [] -> []
                (y:(ys)) -> f (ys) ((y:[]))))

f = (\(ys) (x) -> (case ys of
                    [] -> x
                    (y':(ys'')) -> f (ys'') ((y':(x)))))