module Length where

data Peano = Z | S Peano

eqP :: Peano -> Peano -> Bool
eqP x y = case (x, y) of
  (Z   , Z   ) -> True
  (S x1, S y1) -> eqP x1 y1
  _            -> False

lengthP :: [a] -> Peano
lengthP [] = Z
lengthP (_:xs) = S (lengthP xs)

lengthGen :: Peano -> [Bool]
lengthGen n | lengthP xs `eqP` n = xs
            | otherwise          = [True,False,False]
  where xs free

-- Does not terminate, because interpreter guesses lists of any length
lengthInt :: Int -> [Bool]
lengthInt n | length xs == n = xs
            | otherwise      = [True,False,False]
  where xs free

main :: [Bool]
main = lengthGen Z
