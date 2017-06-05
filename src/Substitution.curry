--- ----------------------------------------------------------------------------
--- This module defines substitutions on type annotated FlatCurry expressions
---
--- @author  Jan Tikovsky
--- @version June 2017
--- ----------------------------------------------------------------------------
module Substitution where

import FiniteMap
import FlatCurry.Annotated.Types

--- Representation of substitutions
type Subst = FM

--- Create an empty substitution
emptySubst :: Ord a => Subst a b
emptySubst = emptyFM (<)

--- Substitute a single variable by an expression
singleSubst :: Ord a => a -> b -> Subst a b
singleSubst = unitFM (<)

--- Create a substitution from a list of variables and a list of expressions
mkSubst :: Ord a => [a] -> [b] -> Subst a b
mkSubst vs es = listToFM (<) (zip vs es)

--- Extend a substitution with the given binding
bind :: Ord a => a -> b -> Subst a b -> Subst a b
bind k v s = addToFM s k v

--- Extract the domain of a given substitution
dom :: Subst a b -> [a]
dom = keysFM

--- Extract the range of a given substitution
range :: Subst a b -> [b]
range = eltsFM

--- Restrict a substitution to a given domain
restrict :: Eq a => [a] -> Subst a b -> Subst a b
restrict vs s = filterFM (\v _ -> v `elem` vs) s

--- substitution on type annotated FlatCurry expressions
type AExpSubst = Subst VarIndex (AExpr TypeExpr)

--- Compose two substitutions s1 and s2. The resulting
--- substitution has the same effect as applying first s2 and afterwards s1
--- on a term
compose :: AExpSubst -> AExpSubst -> AExpSubst
compose s1 s2 = mapFM    (\_ e -> substExp s1 e)          s2 `plusFM`
                filterFM (\v _ -> not (v `elemFM` s2)) s1

--- Find a substitution for a variable with a given default value
findSubstWithDefault :: Ord a => b -> a -> Subst a b -> b
findSubstWithDefault e v s = lookupWithDefaultFM s e v

--- Apply a substitution to a given FlatCurry expression
substExp :: AExpSubst -> AExpr TypeExpr -> AExpr TypeExpr
substExp s v@(AVar       _ i) = findSubstWithDefault v i s
substExp _ l@(ALit       _ _) = l
substExp s (AComb ty ct c es) = AComb ty ct c (map (substExp s) es)
substExp s (ALet     ty bs e) = ALet ty (map substBind bs) (substExp s e)
  where substBind (v, ve) = (v, substExp s ve)
substExp s (AFree    ty vs e) = AFree ty vs (substExp s e)
substExp s (AOr     ty e1 e2) = AOr ty (substExp s e1) (substExp s e2)
substExp s (ACase ty ct e bs) = ACase ty ct (substExp s e) (map substBranch bs)
  where substBranch (ABranch p be) = ABranch p (substExp s be)
substExp s (ATyped  ty e ty') = ATyped ty (substExp s e) ty'

--- substitution on FlatCurry type expressions
type TypeSubst = Subst TVarIndex TypeExpr

--- simple unification on FlatCurry type expression
unify :: [(TypeExpr, TypeExpr)] -> TypeSubst
unify eqs = case unify' emptySubst eqs of
  Nothing  -> error "Given type equations are not unifiable"
  Just mgu -> mgu

unify' :: TypeSubst -> [(TypeExpr, TypeExpr)] -> Maybe TypeSubst
unify' sub []                = Just sub
unify' sub (eq@(t1, t2):eqs) = case eq of
  (TVar i, TVar j) | i == j          -> unify' sub eqs
                   | otherwise       -> replace i t2
  (TVar i,      _) | i `occursIn` t2 -> Nothing
                   | otherwise       -> replace i t2
  (_     , TVar j) | j `occursIn` t1 -> Nothing
                   | otherwise       -> replace j t1
  (FuncType u1 u2, FuncType v1 v2)   -> unify' sub ([(u1, v1), (u2, v2)] ++ eqs)
  (TCons c1 ts1, TCons c2 ts2)
    | c1 == c2 &&
      length ts1 == length ts2       -> unify' sub (zip ts1 ts2 ++ eqs)
    | otherwise                      -> Nothing
  (ForallType _ _, _)                -> error "Substitution.unify: ForallType"
  (_, ForallType _ _)                -> error "Substitution.unify: ForallType"
  _                                  -> Nothing
 where
  replace k v = unify' (bind k v sub)
    (map (\(s, t) -> ( substTy (singleSubst k v) s
                     , substTy (singleSubst k v) t)) eqs)

  occursIn tv ty = case ty of
    TVar           i -> tv == i
    FuncType ty1 ty2 -> tv `occursIn` ty1 || tv `occursIn` ty2
    TCons      _ tys -> any (tv `occursIn`) tys
    ForallType   _ _ -> error "Substitution.occursIn: ForallType"

--- Apply a substitution to a FlatCurry type expression
substTy :: TypeSubst -> TypeExpr -> TypeExpr
substTy s v@(TVar         i) = findSubstWithDefault v i s
substTy s (FuncType ty1 ty2) = FuncType (substTy s ty1) (substTy s ty2)
substTy s (TCons      c tys) = TCons c (map (substTy s) tys)
substTy _ (ForallType   _ _) = error "Substitution.substTy: ForallType"
