module FlatCurryGoodies where

import FlatCurry.Annotated.Goodies
import FlatCurry.Annotated.Pretty  (ppExp)
import FlatCurry.Annotated.Types
import Pretty                      (pPrint)

hasName :: QName -> AFuncDecl a -> Bool
hasName qn f = qn == funcName f

--- Qualification of Prelude functions
prel :: String -> QName
prel f = ("Prelude", f)

--- Annotated FlatCurry expression representing `failed`
failedExpr :: AExpr TypeExpr
failedExpr = AComb (TVar 0) FuncCall ((prel "failed"), TVar 0) []

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

--- Select the pattern variables of a given pattern
patVars :: APattern a -> [(VarIndex, a)]
patVars (APattern _ _ vs) = vs
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

--- Find the matching branch for a given pattern
findBranch :: APattern a -> [ABranchExpr a] -> Maybe (Int, [(VarIndex, a)], AExpr a)
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

combine :: (QName, a) -> (QName, a) -> AExpr a -> [AExpr a] -> [AExpr a] -> AExpr a
combine (amp, an1) (uni, an2) def es1 es2
  | null eqs  = def
  | otherwise = foldr1 (mkCall an1 amp) eqs
  where
  eqs = zipWith (mkCall an2 uni) es1 es2
  mkCall ann qn e1 e2 = AComb ann FuncCall (qn, ann) [e1, e2]

--- Check whether the given FlatCurry program includes a main function
hasMain :: AProg a -> Bool
hasMain (AProg m _ _ fs _) = any (hasName (m, "main")) fs

--- Generate a call for an annotated FlatCurry function
fcall :: TypeExpr -> QName -> [AExpr TypeExpr] -> AExpr TypeExpr
fcall ty qn = AComb ty FuncCall (qn, ty)

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
