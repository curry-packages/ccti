module MList where

-- data Peano = Z | S Peano
--
-- elemAt :: [a] -> Peano -> Maybe a
-- elemAt xs n = fcase xs of
--   []   -> Nothing
--   y:ys -> fcase n of
--             Z   -> Just y
--             S m -> elemAt ys m

main = nthElem [True] 0 -- nthElem ([1,2] :: [Int]) 0

-- simple :: Int -> Bool
-- simple n | n < 42    = True
--          | otherwise = False

-- nthElem :: [a] -> Int -> Maybe a
-- nthElem xs n = case xs of
--   []   -> Nothing
--   y:ys -> case n == 0 of
--             True -> Just y
--             False -> nthElem ys (n-1)

nthElem :: [a] -> Int -> Maybe a
nthElem []       _          = Nothing
nthElem (x : xs) n | n == 0 = Just x
                   | n >  0 = nthElem xs (n - 1)

