module FlatCurryGoodies where

import FlatCurry.Goodies
import FlatCurry.Pretty  (ppExp, defaultOptions)
import FlatCurry.Types
import Pretty            (pPrint)

hasName :: QName -> FuncDecl -> Bool
hasName qn f = qn == funcName f

--- Qualification of Prelude functions
prel :: String -> QName
prel f = ("Prelude", f)

--- FlatCurry expression representing `failed`
failedExpr :: Expr
failedExpr = Comb FuncCall (prel "failed") []

--- FlatCurry expression representing `True`
trueExpr :: Expr
trueExpr = Comb ConsCall (prel "True") []

--- FlatCurry expression representing `False`
falseExpr :: Expr
falseExpr = Comb ConsCall (prel "False") []

--- FlatCurry expression representing `[]`
nil :: Expr
nil = Comb ConsCall (prel "[]") []

--- FlatCurry expression representing `:`
cons :: [Expr] -> Expr
cons = Comb ConsCall (prel ":")

--- FlatCurry expression representing `(,)`
tpl :: [Expr] -> Expr
tpl = Comb ConsCall (prel "(,)")

--- Select the pattern variables of a given pattern
patVars :: Pattern -> [VarIndex]
patVars (Pattern _ vs) = vs
patVars (LPattern   _) = []

--- Get the variable index of a FlatCurry expression
getVarIdx :: Expr -> [VarIndex]
getVarIdx e = case e of
  Var i -> [i]
  _     -> []

--- Check if two pattern are equal
eqPattern :: Pattern -> Pattern -> Bool
eqPattern p q = case (p, q) of
  (Pattern c1 _, Pattern c2 _) -> c1 == c2
  (LPattern  l1, LPattern  l2) -> l1 == l2
  _                            -> False

--- Find the matching branch for a given pattern
findBranch :: Pattern -> [BranchExpr] -> Maybe (Int, [VarIndex], Expr)
findBranch = findBranch' 1
  where
  findBranch' i _ []                                = Nothing
  findBranch' i p (Branch q e : bs) | eqPattern p q = Just (i, patVars q, e)
                                    | otherwise     = findBranch' (i+1) p bs

--- Add an argument to a partial call
addPartCallArg :: CombType -> QName -> [Expr] -> Expr -> Expr
addPartCallArg ct qn es e = Comb ct' qn (es ++ [e])
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

combine :: QName -> QName -> Expr -> [Expr] -> [Expr] -> Expr
combine amp uni def es1 es2
  | null eqs = def
  | otherwise = foldr1 (mkCall amp) eqs
  where
  eqs = zipWith (mkCall uni) es1 es2
  mkCall qn e1 e2 = Comb FuncCall qn [e1, e2]

--- Check whether the given FlatCurry program includes a main function
hasMain :: Prog -> Bool
hasMain (Prog m _ _ fs _) = any (hasName (m, "main")) fs

--- Generate a call for a FlatCurry function
fcall :: QName -> [Expr] -> Expr
fcall = Comb FuncCall

--- Conversion of Curry types into their FlatCurry representation
class ToFCY a where
  toFCY   :: a    -> Expr
  fromFCY :: Expr -> a

instance ToFCY Bool where
  toFCY False = falseExpr
  toFCY True  = trueExpr

  fromFCY e = case e of
    Comb ConsCall ("Prelude", "False") [] -> False
    Comb ConsCall ("Prelude",  "True") [] -> True
    _                                     -> error "fromFCY: no boolean"

instance ToFCY Int where
  toFCY = Lit . Intc

  fromFCY e = case e of
    Lit (Intc x) -> x
    _            -> error "fromFCY: no integer"

instance ToFCY Char where
  toFCY = Lit . Charc

  fromFCY e = case e of
    Lit (Charc c) -> c
    _             -> error "fromFCY: no character"

instance ToFCY Float where
  toFCY = Lit . Floatc

  fromFCY e = case e of
    Lit (Floatc f) -> f
    _              -> error "fromFCY: no float"

instance ToFCY a => ToFCY [a] where
  toFCY []     = nil
  toFCY (x:xs) = cons [toFCY x, toFCY xs]

  fromFCY e = case e of
    Comb ConsCall ("Prelude", "[]") []       -> []
    Comb ConsCall ("Prelude",  ":") [e1, e2] -> fromFCY e1 : fromFCY e2
    _                                        -> error "fromFCY: no list"

instance (ToFCY a, ToFCY b) => ToFCY (a, b) where
  toFCY (x, y) = tpl [toFCY x, toFCY y]

  fromFCY e = case e of
    Comb ConsCall ("Prelude", "(,)") [e1, e2] -> (fromFCY e1, fromFCY e2)
    _                                         -> error $ "fromFCY: no tuple: " ++ show e

-- --- Conversion of FlatCurry types into their Curry representation
-- fromFCYList :: (Expr -> a) -> Expr -> [a]
-- fromFCYList f xs = case xs of
--   Comb ConsCall ("Prelude", "[]") []      -> []
--   Comb ConsCall ("Prelude",  ":") [e1,e2] -> f e1 : fromFCYList f e2
--   _                                       -> error $ "FlatCurryGoodies.fromFCYList: " ++ show xs
--
-- fromFCYChar :: Expr -> Char
-- fromFCYChar e = case e of
--   Lit (Charc c) -> c
--   _             -> error $ "FlatCurryGoodies.fromFCYChar: " ++ show e

--- Pretty print a FlatCurry expression
printExpr :: Expr -> IO ()
printExpr = putStrLn . pPrint . ppExp defaultOptions
