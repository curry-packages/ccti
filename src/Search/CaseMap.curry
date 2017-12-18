module Search.CaseMap where

import qualified FiniteMap as FM
import           List            (intersect, union)

import Language.SMTLIB.Goodies   (tvar)
import Language.SMTLIB.Types     (QIdent, Sort, Term)
import Symbolic                  (BranchNr (..), CoverInfo (..), CoveredCs (..))

--- A context key is a sequence of keys consisting of
--- a primary key and context keys
class (Ord k) => ContextKey k where
  primKey :: k -> Int
  ctxtKey :: k -> k

--- An integer is just a primary key without context keys
instance ContextKey Int where
  primKey = id
  ctxtKey = id

--- A non-empty list of context keys forms a context key:
--- the head element is the primary key
--- the rest of the list is the context key
instance ContextKey a => ContextKey [a] where
  primKey []     = error "ContextKey.primKey: Invalid context key"
  primKey (k:_)  = primKey k
  ctxtKey []     = error "ContextKey.ctxtKey: Invalid context key"
  ctxtKey (_:ks) = ks

--- A map of maps
data ContextMap k v = CM { cm :: FM.FM Int (FM.FM k v) }

--- Abstraction of the concrete implementation of the case map
class CaseMap m where
  --- Create an empty case map
  emptyCM      :: ContextKey k => m k v

  --- Update a case map with the given information
  updateCM     :: ContextKey k => k -> v -> m k v -> m k v

  --- Update a case map with the given information using the given function to combine the old and new value
  updateWithCM :: ContextKey k => (v -> v -> v) -> k -> v -> m k v -> m k v

  --- Lookup a key in the given case map
  lookupCM     :: ContextKey k => k -> m k v -> Maybe v

-- -----------------------------------------------------------------------------
-- Case map implementations
-- -----------------------------------------------------------------------------

--- Check if all branches of a case expression are covered
isCovered :: (ContextKey k, Show k, CaseMap m) => k -> m k CoverInfo -> Bool
isCovered k cm = case lookupCM k cm of
  Nothing -> error $ "CaseMap.isCovered: No coverage info for key " ++ show k
  Just ci -> null $ uncovered ci

--- Get the covered constructors / constraints of a case expression
covered :: (ContextKey k, Show k, CaseMap m) => k -> m k CoverInfo -> CoveredCs
covered k cm = case lookupCM k cm of
    Nothing -> error $ "CaseMap.covered: No coverage info for key " ++ show k
    Just ci -> coveredCs ci

--- Cover the given branch for a case expression in a case map
cover :: (ContextKey k, CaseMap m) => k -> BranchNr -> CoveredCs
      -> m k CoverInfo -> m k CoverInfo
cover k (BNr m n) cc cm = updateWithCM combInfo k info cm
 where
  info = CoverInfo [b | b <- [1 .. n], b /= m] cc

  --- Combine coverage information
  combInfo :: CoverInfo -> CoverInfo -> CoverInfo
  combInfo (CoverInfo u1 cc1) (CoverInfo u2 cc2)
    = let cc' = case (cc1, cc2) of
            (CCons      cs1, CCons   cs2) -> CCons (cs1 `union` cs2)
            (CConstr lc1 l1, CConstr _ _) -> CConstr lc1 l1
            _                      ->
              error "CaseMap.combInfo: Incompatible coverage info"
      in CoverInfo (u1 `intersect` u2) cc'

--- Cover all branches of a case expression
coverAll :: (ContextKey k, CaseMap m) => k -> m k CoverInfo -> m k CoverInfo
coverAll k cm = case lookupCM k cm of
  Nothing               -> cm
  Just (CoverInfo _ cc) -> updateCM k (CoverInfo [] cc) cm

-- -----------------------------------------------------------------------------
-- Case map instances
-- -----------------------------------------------------------------------------

instance CaseMap FM.FM where
  emptyCM = FM.emptyFM (<)

  updateCM k v cm = FM.addToFM cm k v

  updateWithCM f k v cm = FM.addToFM_C f cm k v

  lookupCM = flip FM.lookupFM

instance CaseMap ContextMap where
  emptyCM = CM (FM.emptyFM (<))

  updateCM k v m =
    CM $ FM.addToFM_C FM.plusFM (cm m) (primKey k) (FM.unitFM (<) (ctxtKey k) v)

  updateWithCM f k v m =
    CM $ FM.addToFM_C (FM.plusFM_C f) (cm m) (primKey k) (FM.unitFM (<) (ctxtKey k) v)

  lookupCM k m = case FM.lookupFM (cm m) (primKey k) of
    Nothing -> Nothing
    Just fm -> FM.lookupFM fm (ctxtKey k)
