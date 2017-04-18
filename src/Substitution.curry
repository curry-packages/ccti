--- ----------------------------------------------------------------------------
--- This module defines substitutions on FlatCurry expressions
---
--- @author  Jan Tikovsky
--- @version March 2017
--- ----------------------------------------------------------------------------
module Substitution where

import FiniteMap
import FlatCurry.Types

--- Representation of substitutions
type Subst = FM VarIndex Expr

--- Create an empty substitution
emptySubst :: Subst
emptySubst = emptyFM (<)

--- Substitute a single variable by an expression
singleSubst :: VarIndex -> Expr -> Subst
singleSubst = unitFM (<)

--- Create a substitution from a list of variables and a list of expressions
mkSubst :: [VarIndex] -> [Expr] -> Subst
mkSubst vs es = listToFM (<) (zip vs es)

--- Extract the domain of a given substitution
dom :: Subst -> [VarIndex]
dom = keysFM

--- Extract the range of a given substitution
range :: Subst -> [Expr]
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
findSubstWithDefault :: Expr -> VarIndex -> Subst -> Expr
findSubstWithDefault e v s = lookupWithDefaultFM s e v

--- Apply a substitution to a given FlatCurry expression
subst :: Subst -> Expr -> Expr
subst s v@(Var      i) = findSubstWithDefault v i s
subst _ l@(Lit      _) = l
subst s (Comb ct c es) = Comb ct c (map (subst s) es)
subst s (Let     bs e) = Let (map substBind bs) (subst s e)
  where substBind (v, ve) = (v, subst s ve)
subst s (Free    vs e) = Free vs (subst s e)
subst s (Or     e1 e2) = Or (subst s e1) (subst s e2)
subst s (Case ct e bs) = Case ct (subst s e) (map substBranch bs)
  where substBranch (Branch p be) = Branch p (subst s be)
subst s (Typed   e ty) = Typed (subst s e) ty
