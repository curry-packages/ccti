module PriorityQueue where

import FiniteMap
import List      (union)
import Utils

type PQueue a b = FM a [b]

emptyPQ :: Ord a => PQueue a b
emptyPQ = emptyFM (<)

--- Get next element according to priority
getElem :: PQueue a b -> Maybe b
getElem = fmap (head . snd) . minFM

--- Add an element to the queue
addElem :: (Eq a, Eq b) => a -> b -> PQueue a b -> PQueue a b
addElem k v pq = addToFM_C union pq k [v]

--- Remove an element from the queue
-- rather inefficient
rmvElem :: Eq a => a -> PQueue a b -> PQueue a b
rmvElem k pq = case lookupFM pq k of
  Nothing -> pq
  Just vs | length vs <= 1 -> delFromFM pq k
          | otherwise      -> updFM pq k tail

