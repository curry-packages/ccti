module Search.Queue where

import qualified Data.PQ as PQ

--- Abstraction of the concrete implementation of the search queue
class SearchQueue q where
  --- Create an empty search queue
  emptySQ   :: q k v

  --- Add an element to the search queue under the given key
  enqueueSQ :: (Ord k) => k -> v -> q k v -> q k v

  --- Remove the first element of the given search queue if any
  dequeueSQ :: (Ord k, Eq v) => q k v -> Maybe (v, q k v)

-- -----------------------------------------------------------------------------
-- Instances of the search queue
-- -----------------------------------------------------------------------------

instance SearchQueue PQ.PQ where
  emptySQ   = PQ.emptyPQ
  enqueueSQ = PQ.enqueue
  dequeueSQ = PQ.dequeue
