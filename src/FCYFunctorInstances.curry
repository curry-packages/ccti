--- ----------------------------------------------------------------------------
--- This module provides Functor instances for annotated FlatCurry
---
--- @author  Jan Tikovsky
--- @version August 2017
--- ----------------------------------------------------------------------------
module FCYFunctorInstances where

import FlatCurry.Annotated.Types

import Utils                     (mapSnd)

instance Functor AProg where
  fmap f (AProg m imps ts fs os) = AProg m imps ts (map (fmap f) fs) os

instance Functor AFuncDecl where
  fmap f (AFunc qn a vis ty r) = AFunc qn a vis ty (fmap f r)

instance Functor ARule where
  fmap f (ARule     ann vs e) = ARule (f ann) (map (mapSnd f) vs) (fmap f e)
  fmap f (AExternal ann    n) = AExternal (f ann) n

instance Functor AExpr where
  fmap f (AVar   ann        vi) = AVar (f ann) vi
  fmap f (ALit   ann         l) = ALit (f ann) l
  fmap f (AComb  ann ct aqn es) = AComb (f ann) ct (mapSnd f aqn) (map (fmap f) es)
  fmap f (ALet   ann      ds e) = ALet (f ann) (map appF ds) (fmap f e)
    where appF (avi, ei) = (mapSnd f avi, fmap f ei)
  fmap f (AFree  ann      bs e) = AFree (f ann) (map (mapSnd f) bs) (fmap f e)
  fmap f (AOr    ann     e1 e2) = AOr (f ann) (fmap f e1) (fmap f e2)
  fmap f (ACase  ann   ct e bs) = ACase (f ann) ct (fmap f e) (map (fmap f) bs)
  fmap f (ATyped ann      e ty) = ATyped (f ann) (fmap f e) ty

instance Functor ABranchExpr where
  fmap f (ABranch p e) = ABranch (fmap f p) (fmap f e)

instance Functor APattern where
  fmap f (APattern  ann aqn ps) = APattern (f ann) (mapSnd f aqn) (map (mapSnd f) ps)
  fmap f (ALPattern ann      l) = ALPattern (f ann) l
