--- ----------------------------------------------------------------------------
--- This module provides some additional goodies for annotated FlatCurry.
---
--- @author  Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module FlatCurryGoodies where

import FlatCurry.Annotated.Goodies
import FlatCurry.Annotated.Pretty  (ppExp)
import FlatCurry.Annotated.Types
import List                        (find)
import PrettyPrint

--- Qualified name of a FlatCurry constructor annotated with type information
data TypedFCYCons = TFCYCons QName TypeExpr
  deriving Show

instance Pretty TypedFCYCons where
  pretty (TFCYCons qn ty) = ppQName qn <+> doubleColon <+> ppTypeExp ty

--- Consider only the qualified name when comparing two typed FlatCurry
--- constructors
instance Eq TypedFCYCons where
  (TFCYCons qn1 _) == (TFCYCons qn2 _) = qn1 == qn2

--- Consider only the qualified name when comparing two typed FlatCurry
--- constructors
instance Ord TypedFCYCons where
  compare (TFCYCons qn1 _) (TFCYCons qn2 _) = compare qn1 qn2

--- Create a typed FlatCurry 'Prelude' constructor
prelTFCYCons :: String -> TypeExpr -> TypedFCYCons
prelTFCYCons c ty = TFCYCons (prel c) ty

--- Generate a functional FlatCurry type expression from a list of argument types
--- and a result type
mkFunType :: [TypeExpr] -> TypeExpr -> TypeExpr
mkFunType tys ty = foldr FuncType ty tys

hasName :: QName -> AFuncDecl a -> Bool
hasName qn f = qn == funcName f

--- Qualification of Prelude functions
prel :: String -> QName
prel f = ("Prelude", f)

--- Annotated FlatCurry expression representing `failed`
failedExpr :: TypeExpr -> AExpr TypeExpr
failedExpr ty = AComb ty FuncCall ((prel "failed"), ty) []

--- smart constructor for a nondeterministic choice in FlatCurry
mkOr :: AExpr TypeExpr -> AExpr TypeExpr -> AExpr TypeExpr
mkOr e1 e2 | e1 == failedExpr ty = e2
           | e2 == failedExpr ty = e1
           | otherwise           = AOr ty e1 e2
  where ty = annExpr e1

--- Annotated FlatCurry type expression representing `Bool`
boolType :: TypeExpr
boolType = TCons (prel "Bool") []

--- Annotated FlatCurry type expression representing `Int`
intType :: TypeExpr
intType = TCons (prel "Int") []

--- Annotated FlatCurry type expression representing `Char`
charType :: TypeExpr
charType = TCons (prel "Char") []

--- Annotated FlatCurry type expression representing `Float`
floatType :: TypeExpr
floatType = TCons (prel "Float") []

--- Annotated FlatCurry type expression representing `Bool -> Bool -> Bool`
ampType :: TypeExpr
ampType = FuncType boolType (FuncType boolType boolType)

--- Annotated FlatCurry type expression representing `a -> a -> Bool`
unifyType :: TypeExpr -> TypeExpr
unifyType ty = FuncType ty (FuncType ty boolType)

--- Annotated FlatCurry type expression representing `Bool -> a -> a`
condType :: TypeExpr -> TypeExpr
condType ty = FuncType boolType (FuncType ty ty)

--- Annotated FlatCurry expression representing `True`
trueExpr :: AExpr TypeExpr
trueExpr = AComb boolType ConsCall (prel "True", boolType) []

--- Annotated FlatCurry expression representing `False`
falseExpr :: AExpr TypeExpr
falseExpr = AComb boolType ConsCall (prel "False", boolType) []

--- Annotated FlatCurry type constructor for lists
listType :: TypeExpr -> TypeExpr
listType ty = TCons (prel "[]") [ty]

--- Annotated FlatCurry type constructor for tuples
tplType :: [TypeExpr] -> TypeExpr
tplType tys = TCons (prel "(,)") tys

--- smart constructor for typed tuple constructors in FlatCurry
mkTplType :: String -> Int -> TypedFCYCons
mkTplType n a | a >= 2 = TFCYCons qn (mkFunType tvars (TCons qn tvars))
  where qn    = prel n
        tvars = map TVar [0 .. a-1]

--- Annotated FlatCurry type expression representing `Bool`
unitType :: TypeExpr
unitType = TCons (prel "()") []

--- Annotated FlatCurry expression representing `()`
unit :: AExpr TypeExpr
unit = AComb unitType ConsCall (prel "()", unitType) []

--- Annotated FlatCurry expression representing `[]`
nil :: AExpr TypeExpr
nil = AComb ty  ConsCall (prel "[]", ty) []
  where ty = listType (TVar 0)

--- Annotated FlatCurry expression representing `:`
cons :: TypeExpr -> [AExpr TypeExpr] -> AExpr TypeExpr
cons ty = AComb lty ConsCall ((prel ":"), lty)
  where lty = listType ty

--- FlatCurry expression representing `(,)`
tpl :: TypeExpr -> [AExpr TypeExpr] -> AExpr TypeExpr
tpl ty = AComb ty ConsCall ((prel "(,)"), ty)

--- Get argument and result types of a given type signature
getTypes :: TypeExpr -> [TypeExpr]
getTypes ty@(TVar         _) = [ty]
getTypes ty@(TCons      _ _) = [ty]
getTypes (FuncType  ty1 ty2) = ty1 : getTypes ty2
getTypes (ForallType    _ _) = error "FlatCurryGoodies.getTypes: forall"

--- Select the pattern variables of a given pattern
patVars :: APattern a -> [VarIndex]
patVars (APattern _ _ vs) = map fst vs
patVars (ALPattern   _ _) = []

--- Get the variable index of a FlatCurry expression
getVarIdx :: AExpr a -> [VarIndex]
getVarIdx e = case e of
  AVar _ i -> [i]
  _        -> []

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
combine :: QName -> QName -> AExpr TypeExpr -> [AExpr TypeExpr]
        -> [AExpr TypeExpr] -> AExpr TypeExpr
combine amp uni def es1 es2
  | null eqs  = def
  | otherwise = foldr1 (mkCall (const ampType) amp) eqs
  where
  eqs                = zipWith (mkCall unifyType uni) es1 es2
  mkCall tc qn e1 e2 = AComb boolType FuncCall (qn, tc (annExpr e1)) [e1, e2]

--- Get the rhs expression of the main function of the given FlatCurry program
getMainBody :: AProg a -> Maybe (AExpr a)
getMainBody (AProg m _ _ fs _) = case findFunc (m, "main") fs of
  Just f | isFuncCall b -> Just b
    where b = funcBody f
  _                     -> Nothing

--- Generate a call of the main function of the given module
mainCall :: String -> TypeExpr -> AExpr TypeExpr
mainCall m ty = AComb (resultType ty) FuncCall ((m, "main"), ty) []

--- Conversion of Curry types into their FlatCurry representation
class ToFCY a where
  toFCY   :: a    -> AExpr TypeExpr
  fromFCY :: AExpr TypeExpr -> a

instance ToFCY Bool where
  toFCY False = falseExpr
  toFCY True  = trueExpr

  fromFCY e = case e of
    AComb _ ConsCall (("Prelude", "False"), _) [] -> False
    AComb _ ConsCall (("Prelude",  "True"), _) [] -> True
    _                                             -> error "fromFCY: no boolean"

instance ToFCY Int where
  toFCY = ALit intType . Intc

  fromFCY e = case e of
    ALit _ (Intc x) -> x
    _               -> error "fromFCY: no integer"

instance ToFCY Char where
  toFCY = ALit charType . Charc

  fromFCY e = case e of
    ALit _ (Charc c) -> c
    _                -> error "fromFCY: no character"

instance ToFCY Float where
  toFCY = ALit floatType . Floatc

  fromFCY e = case e of
    ALit _ (Floatc f) -> f
    _                 -> error "fromFCY: no float"

instance ToFCY a => ToFCY [a] where
  toFCY []     = nil
  toFCY (x:xs) = cons ty [x', toFCY xs]
    where x' = toFCY x
          ty = annExpr x'

  fromFCY e = case e of
    AComb _ ConsCall (("Prelude", "[]"), _) []       -> []
    AComb _ ConsCall (("Prelude",  ":"), _) [e1, e2] -> fromFCY e1 : fromFCY e2
    _                                                -> error "fromFCY: no list"

instance (ToFCY a, ToFCY b) => ToFCY (a, b) where
  toFCY (x, y) = tpl tty [x', y']
    where x'  = toFCY x
          y'  = toFCY y
          tty = tplType [annExpr x', annExpr y']

  fromFCY e = case e of
    AComb _ ConsCall (("Prelude", "(,)"), _) [e1, e2] -> (fromFCY e1, fromFCY e2)
    _                                                 -> error $ "fromFCY: no tuple: " ++ show e

--- Pretty print a FlatCurry expression
printExpr :: AExpr TypeExpr -> IO ()
printExpr = putStrLn . pPrint . ppExp
