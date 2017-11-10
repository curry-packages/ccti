--- ----------------------------------------------------------------------------
--- This module provides a simple implementation of a priority queue with
--- pairing heaps in Curry.
---
--- @author  Jan Tikovsky
--- @version October 2017
--- ----------------------------------------------------------------------------
module Data.PQ (PQ, emptyPQ, findMin, enqueue, dequeue, merge) where

import Data.DList
import Text.Pretty

--- Priority queue implementation with pairing heaps
data PQ k v = Empty
            | PQ k (DList v) [PQ k v]
 deriving Show

--- Create an empty priority queue
emptyPQ :: PQ k v
emptyPQ = Empty

--- Get the element with the minimum key from the priority queue
findMin :: Eq v => PQ k v -> Maybe v
findMin Empty       = Nothing
findMin (PQ _ vs _) = case toListNub vs of
  []    -> error "Data.PQ.findMin: Unexpected empty value list"
  (w:_) -> Just w

--- Add an element to the priority queue using the given key
enqueue :: Ord k => k -> v -> PQ k v -> PQ k v
enqueue k v = merge (PQ k (singleton v) [])

--- Remove an element from the priority queue and return it together with the remaining queue
dequeue :: (Ord k, Eq v) => PQ k v -> Maybe (v, PQ k v)
dequeue Empty        = Nothing
dequeue (PQ k vs hs) = case toListNub vs of
  []     -> error "Data.PQ.dequeue: Unexpected empty value list"
  [w]    -> Just (w, mergePairs hs)
  (w:ws) -> Just (w, PQ k (fromList ws) hs)

--- Merge two priority queues
merge :: Ord k => PQ k v -> PQ k v -> PQ k v
merge h1 h2 = case (h1, h2) of
  (Empty       ,            _) -> h2
  (_           ,        Empty) -> h1
  (PQ k1 v1 hs1, PQ k2 v2 hs2)
    | k1 <  k2                 -> PQ k1 v1             (h2 : hs1)
    | k1 == k2                 -> PQ k1 (append v1 v2) (hs1 ++ hs2)
    | otherwise                -> PQ k2 v2             (h1 : hs2)

-- helper

mergePairs :: Ord k => [PQ k v] -> PQ k v
mergePairs hs = case hs of
  []         -> Empty
  [h]        -> h
  (h1:h2:gs) -> merge (merge h1 h2) (mergePairs gs)

-- pretty printing

instance (Pretty k, Show v) => Pretty (PQ k v) where
  pretty Empty        = text "<>"
  pretty (PQ k vs hs) = angles (pretty k <> colon <+> list (map (text . show) (toList vs)) <+> list (map pretty hs))
