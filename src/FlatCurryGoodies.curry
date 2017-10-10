--- ----------------------------------------------------------------------------
--- This module provides some additional goodies for annotated FlatCurry.
---
--- @author  Jan Tikovsky
--- @version October 2017
--- ----------------------------------------------------------------------------
module FlatCurryGoodies where

import FlatCurry.Annotated.Goodies
import FlatCurry.Annotated.Pretty  (ppExp)
import FlatCurry.Annotated.Types
import List                        (find, isPrefixOf)

import Utils

--- ----------------------------------------------------------------------------
--- Extended annotations for concolic testing
--- ----------------------------------------------------------------------------

--- Extended FlatCurry annotation for concolic testing providing
---   * case identifiers and
---   * literal flags
type IDAnn a = (a, Maybe VarIndex, Bool)

--- Concolic testing annotation with type information
type TypeAnn = IDAnn TypeExpr

--- Extend given annotation to default `IDAnn` annotation
extendAnn :: a -> IDAnn a
extendAnn ann = (ann, Nothing, False)

--- Select type annotation
tyAnn :: TypeAnn -> TypeExpr
tyAnn = fst3

--- Select case identifier annotation
cidAnn :: TypeAnn -> VarIndex
cidAnn ann = case snd3 ann of
  Nothing  -> error "FlatCurryGoodies.cidAnn: No identifier annotation found"
  Just cid -> cid

--- Select literal annotation
litAnn :: TypeAnn -> Bool
litAnn = trd3

--- ----------------------------------------------------------------------------

--- Generate a functional FlatCurry type expression from a list of argument types
--- and a result type
mkFunType :: [TypeExpr] -> TypeExpr -> TypeExpr
mkFunType tys ty = foldr FuncType ty tys

hasName :: QName -> AFuncDecl a -> Bool
hasName qn f = qn == funcName f

--- Qualification of Prelude functions
prel :: String -> QName
prel f = ("Prelude", f)

--- Qualify first component of a tuple with "Prelude"
qualPrel :: (String, b) -> (QName, b)
qualPrel (x, y) = (prel x, y)

--- Check whether the qualified name represents a dictionary
isDict :: QName -> Bool
isDict (_, n) = "_Dict" `isPrefixOf` n

--- Annotated FlatCurry expression representing `failed`
failedExpr :: TypeAnn -> AExpr TypeAnn
failedExpr ann = AComb ann FuncCall ((prel "failed"), ann) []

--- smart constructor for a nondeterministic choice in FlatCurry
mkOr :: AExpr TypeAnn -> AExpr TypeAnn -> AExpr TypeAnn
mkOr e1 e2 | e1 == failedExpr ann = e2
           | e2 == failedExpr ann = e1
           | otherwise            = AOr ann e1 e2
  where ann = annExpr e1

--- ----------------------------------------------------------------------------
--- Representation of some basic types in FlatCurry
--- ----------------------------------------------------------------------------

boolType :: TypeExpr
boolType = TCons (prel "Bool") []

intType :: TypeExpr
intType = TCons (prel "Int") []

charType :: TypeExpr
charType = TCons (prel "Char") []

floatType :: TypeExpr
floatType = TCons (prel "Float") []

listType :: TypeExpr -> TypeExpr
listType ty = TCons (prel "[]") [ty]

--- Extended annotation for `()`
unitType :: TypeExpr
unitType = TCons (prel "()") []

--- ----------------------------------------------------------------------------
--- Representation of extended annotations
--- ----------------------------------------------------------------------------

--- Extended annotation for `Bool`
boolAnn :: TypeAnn
boolAnn = extendAnn boolType

--- Extended annotation for `Int`
intAnn :: TypeAnn
intAnn = extendAnn intType

--- Extended annotation for `Char`
charAnn :: TypeAnn
charAnn = extendAnn $ TCons (prel "Char") []

--- Extended annotation for `Float`
floatAnn :: TypeAnn
floatAnn = extendAnn floatType

--- Extended annotation for the list type constructor
listAnn :: TypeExpr -> TypeAnn
listAnn = extendAnn . listType

--- Extended annotation for the tuple type constructor
tplAnn :: [TypeExpr] -> TypeAnn
tplAnn tys = extendAnn $ TCons (prel "(,)") tys

--- Extended annotations of Prelude functions

--- Extended annotation for type `Bool -> a -> a`
condAnn :: TypeExpr -> TypeAnn
condAnn ty = extendAnn $ FuncType boolType (FuncType ty ty)

--- Extended annotation for type `Bool -> Bool -> Bool`
ampAnn :: TypeAnn
ampAnn = extendAnn $ FuncType boolType (FuncType boolType boolType)

--- Extended annotation for type `a -> a -> Bool`
unifyAnn :: TypeAnn -> TypeAnn
unifyAnn ann = extendAnn $ FuncType ty (FuncType ty boolType)
  where ty = tyAnn ann

--- ----------------------------------------------------------------------------
--- FlatCurry expressions
--- ----------------------------------------------------------------------------

--- Annotated FlatCurry expression representing `True`
trueExpr :: AExpr TypeAnn
trueExpr = AComb boolAnn ConsCall (prel "True", boolAnn) []

--- Annotated FlatCurry expression representing `False`
falseExpr :: AExpr TypeAnn
falseExpr = AComb boolAnn ConsCall (prel "False", boolAnn) []

--- Annotated FlatCurry expression representing `[]`
nil :: AExpr TypeAnn
nil = AComb ann  ConsCall (prel "[]", ann) []
  where ann = listAnn (TVar 0)

--- Annotated FlatCurry expression representing `:`
cons :: TypeExpr -> [AExpr TypeAnn] -> AExpr TypeAnn
cons ty = AComb ann ConsCall ((prel ":"), ann)
  where ann = listAnn ty

--- FlatCurry expression representing `(,)`
tpl :: [TypeExpr] -> [AExpr TypeAnn] -> AExpr TypeAnn
tpl tys = AComb ann ConsCall ((prel "(,)"), ann)
  where ann = tplAnn tys

--- Check if the given FlatCurry type is a boolean type
isBoolType :: TypeExpr -> Bool
isBoolType ty = ty == boolType

--- Check if the given FlatCurry expression is `otherwise`
isOtherwise :: AExpr a -> Bool
isOtherwise e = case e of
  AComb _ FuncCall (("Prelude", "otherwise"), _) [] -> True
  _                                                 -> False

hasBoolType :: AExpr TypeAnn -> Bool
hasBoolType e = tyAnn (annExpr e) == boolType

--- Check if the given FlatCurry expression is a boolean conjunction
isConj :: AExpr a -> Bool
isConj e = case e of
  AComb _ _ (qn, _) _ -> qn == prel "&&" || qn == prel "&"
  _                   -> False

--- Check if the given FlatCurry expression is a boolean disjunction
isDisj :: AExpr a -> Bool
isDisj e = case e of
  AComb _ _ (qn, _) _ -> qn == prel "||"
  _                   -> False

--- ----------------------------------------------------------------------------
--- FlatCurry pattern
--- ----------------------------------------------------------------------------

--- FlatCurry `True` pattern
truePat :: APattern TypeAnn
truePat = APattern boolAnn (prel "True", boolAnn) []

--- FlatCurry `False` pattern
falsePat :: APattern TypeAnn
falsePat = APattern boolAnn (prel "False", boolAnn) []

--- Get the result type followed by the argument types in given order
resArgTypes :: TypeExpr -> [TypeExpr]
resArgTypes = resArgTypes' []
  where
  resArgTypes' tys ty@(TVar         _) = ty : reverse tys
  resArgTypes' tys ty@(TCons      _ _) = ty : reverse tys
  resArgTypes' tys (FuncType  ty1 ty2) = resArgTypes' (ty1 : tys) ty2
  resArgTypes' _   (ForallType    _ _) = error "FlatCurryGoodies.resArgTypes"

--- Select the pattern variables of a given pattern
patVars :: APattern a -> [VarIndex]
patVars (APattern _ _ vs) = map fst vs
patVars (ALPattern   _ _) = []

--- Get the variable index of a FlatCurry expression
getVarIdx :: AExpr a -> [VarIndex]
getVarIdx e = case e of
  AVar _ i -> [i]
  _        -> []

--- Get all type variables of a given FlatCurry type expression
getTyVars :: TypeExpr -> [TVarIndex]
getTyVars (TVar           i) = [i]
getTyVars (FuncType ty1 ty2) = getTyVars ty1 ++ getTyVars ty2
getTyVars (TCons      _ tys) = concatMap getTyVars tys
getTyVars (ForallType   _ _) = []

--- Check if two pattern are equal
eqPattern :: APattern a -> APattern a -> Bool
eqPattern p q = case (p, q) of
  (APattern _ (c1, _) _, APattern _ (c2, _) _) -> c1 == c2
  (ALPattern       _ l1, ALPattern       _ l2) -> l1 == l2
  _                                            -> False

--- Find the function declaration for given qualified name
findFunc :: QName -> [AFuncDecl a] -> Maybe (AFuncDecl a)
findFunc qn fs = find (hasName qn) fs

--- Find the matching branch for a given pattern
findBranch :: APattern a -> [ABranchExpr a] -> Maybe (Int, [VarIndex], AExpr a)
findBranch = findBranch' 1
  where
  findBranch' _ _ []                                 = Nothing
  findBranch' i p (ABranch q e : bs) | eqPattern p q = Just (i, patVars q, e)
                                     | otherwise     = findBranch' (i+1) p bs

--- Add an argument to a partial call
addPartCallArg :: a -> CombType -> (QName, a) -> [AExpr a] -> AExpr a -> AExpr a
addPartCallArg ann ct qn es e = AComb ann ct' qn (es ++ [e])
  where ct' = case ct of
                ConsPartCall 1 -> ConsCall
                ConsPartCall n -> ConsPartCall (n - 1)
                FuncPartCall 1 -> FuncCall
                FuncPartCall n -> FuncPartCall (n - 1)
                _              -> error "FlatCurryGoodies.addPartCallArg"

--- Check if given combination type is a partial call
isPartCall :: CombType -> Bool
isPartCall ct = case ct of
  ConsPartCall _ -> True
  FuncPartCall _ -> True
  _              -> False

--- Combine given expressions lists with `=:=` or `=:<=` and resulting
--- unifications with `&`
combine :: QName -> QName -> AExpr TypeAnn -> [AExpr TypeAnn]
        -> [AExpr TypeAnn] -> AExpr TypeAnn
combine amp uni def es1 es2
  | null eqs  = def
  | otherwise = foldr1 (mkCall (const ampAnn) amp) eqs
  where
  eqs                = zipWith (mkCall unifyAnn uni) es1 es2
  mkCall tc qn e1 e2 = AComb boolAnn FuncCall (qn, tc (annExpr e1)) [e1, e2]

--- Get the rhs expression of the main function of the given FlatCurry program
getMainBody :: AProg a -> Maybe (AExpr a)
getMainBody (AProg m _ _ fs _) = case findFunc (m, "main") fs of
  Just f | isFuncCall b -> Just b
    where b = funcBody f
  _                     -> Nothing

--- Generate a call of the main function of the given module
-- mainCall :: String -> TypeExpr -> AExpr TypeExpr
-- mainCall m ty = AComb (resultType ty) FuncCall ((m, "main"), ty) []

--- Conversion of Curry types into their FlatCurry representation
class ToFCY a where
  toFCY   :: a    -> AExpr TypeAnn
  fromFCY :: AExpr TypeAnn -> a

instance ToFCY Bool where
  toFCY False = falseExpr
  toFCY True  = trueExpr

  fromFCY e = case e of
    AComb _ ConsCall (("Prelude", "False"), _) [] -> False
    AComb _ ConsCall (("Prelude",  "True"), _) [] -> True
    _                                             -> error "fromFCY: no boolean"

instance ToFCY Int where
  toFCY = ALit intAnn . Intc

  fromFCY e = case e of
    ALit _ (Intc x) -> x
    _               -> error "fromFCY: no integer"

instance ToFCY Char where
  toFCY = ALit charAnn . Charc

  fromFCY e = case e of
    ALit _ (Charc c) -> c
    _                -> error "fromFCY: no character"

instance ToFCY Float where
  toFCY = ALit floatAnn . Floatc

  fromFCY e = case e of
    ALit _ (Floatc f) -> f
    _                 -> error "fromFCY: no float"

instance ToFCY a => ToFCY [a] where
  toFCY []     = nil
  toFCY (x:xs) = cons ty [x', toFCY xs]
    where x' = toFCY x
          ty = tyAnn (annExpr x')

  fromFCY e = case e of
    AComb _ ConsCall (("Prelude", "[]"), _) []       -> []
    AComb _ ConsCall (("Prelude",  ":"), _) [e1, e2] -> fromFCY e1 : fromFCY e2
    _                                                -> error "fromFCY: no list"

instance (ToFCY a, ToFCY b) => ToFCY (a, b) where
  toFCY (x, y) = tpl tys [x', y']
    where x'  = toFCY x
          y'  = toFCY y
          tys = map (tyAnn . annExpr) [x', y']

  fromFCY e = case e of
    AComb _ ConsCall (("Prelude", "(,)"), _) [e1, e2] -> (fromFCY e1, fromFCY e2)
    _                                                 -> error $ "fromFCY: no tuple: " ++ show e
