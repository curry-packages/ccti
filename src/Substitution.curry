--- ----------------------------------------------------------------------------
--- This module defines substitutions on type annotated FlatCurry expressions
---
--- @author  Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module Substitution where

import FiniteMap
import FlatCurry.Annotated.Types

--- Representation of substitutions
type Subst = FM VarIndex (AExpr TypeExpr)

--- Create an empty substitution
emptySubst :: Subst
emptySubst = emptyFM (<)

--- Substitute a single variable by an expression
singleSubst :: VarIndex -> AExpr TypeExpr -> Subst
singleSubst = unitFM (<)

--- Create a substitution from a list of variables and a list of expressions
mkSubst :: [VarIndex] -> [AExpr TypeExpr] -> Subst
mkSubst vs es = listToFM (<) (zip vs es)

--- Extract the domain of a given substitution
dom :: Subst -> [VarIndex]
dom = keysFM

--- Extract the range of a given substitution
range :: Subst -> [AExpr TypeExpr]
range = eltsFM

--- Restrict a substitution to a given domain
restrict :: [VarIndex] -> Subst -> Subst
restrict vs s = filterFM (\v _ -> v `elem` vs) s

--- Compose two substitutions s1 and s2. The resulting
--- substitution has the same effect as applying first s2 and afterwards s1
--- on a term
compose :: Subst -> Subst -> Subst
compose s1 s2 = mapFM    (\_ e -> subst s1 e)          s2 `plusFM`
                filterFM (\v _ -> not (v `elemFM` s2)) s1

--- Find a substitution for a variable with a given default value
findSubstWithDefault :: AExpr TypeExpr -> VarIndex -> Subst -> AExpr TypeExpr
findSubstWithDefault e v s = lookupWithDefaultFM s e v

--- Apply a substitution to a given FlatCurry expression
subst :: Subst -> AExpr TypeExpr -> AExpr TypeExpr
subst s v@(AVar       _ i) = findSubstWithDefault v i s
subst _ l@(ALit       _ _) = l
subst s (AComb ty ct c es) = AComb ty ct c (map (subst s) es)
subst s (ALet     ty bs e) = ALet ty (map substBind bs) (subst s e)
  where substBind (v, ve) = (v, subst s ve)
subst s (AFree    ty vs e) = AFree ty vs (subst s e)
subst s (AOr     ty e1 e2) = AOr ty (subst s e1) (subst s e2)
subst s (ACase ty ct e bs) = ACase ty ct (subst s e) (map substBranch bs)
  where substBranch (ABranch p be) = ABranch p (subst s be)
subst s (ATyped  ty e ty') = ATyped ty (subst s e) ty'
