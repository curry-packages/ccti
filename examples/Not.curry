module Not where

elemAt :: [Bool] -> Peano -> Maybe Bool
elemAt xs n = case xs of
  []   -> Nothing
  y:ys -> case n of
            Z   -> Just y
            S m -> elemAt ys m

last :: [a] -> a
last xs = case xs of
  [y]    -> y
  _:y:ys -> last (y:ys)

neg :: Bool -> Bool
neg False = True
neg True = False

data Peano = Z | S Peano

add :: Peano -> Peano -> Peano
add Z     q = q
add (S p) q = add p (S q)

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

head :: [a] -> a
head (x:_) = x

main = elemAt [True, False] (S Z) --(head ? last) $ map neg [True, False]
