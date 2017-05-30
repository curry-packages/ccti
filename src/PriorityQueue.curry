module PriorityQueue where

import FiniteMap

type PQueue a b = FM a [b]

emptyPQ :: Ord a => PQueue a b
emptyPQ = emptyFM (<)

--- Get next element according to priority
getElem :: PQueue a b -> Maybe b
getElem = fmap (head . snd) . minFM

--- Add an element to the queue
addElem :: Eq a => a -> b -> PQueue a b -> PQueue a b
addElem k v pq = addToFM_C (++) pq k [v]

--- Remove an element from the queue
-- rather inefficient
rmvElem :: Eq a => a -> PQueue a b -> PQueue a b
rmvElem k pq = case lookupFM pq k of
  Nothing -> pq
  Just vs | length vs <= 1 -> delFromFM pq k
          | otherwise      -> updFM pq k tail

-- TODO: Remove when there is a functor instance for Maybe
instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)
