take :: Int -> [a] -> [a]
take n xs = case n <= 0 of True  -> []
                           False -> case xs of []   -> []
                                               y:ys -> y : take (n-1) ys

take2 :: Int -> [a] -> [a]
take2 n xs = case n > 0 of False -> []
                           True  -> case xs of []   -> []
                                               y:ys -> y : take2 (n-1) ys


main = take2 0 [True, False]
