--- ----------------------------------------------------------------------------
--- This module provides transforms FlatCurry programs for concolic evaluation:
---   - For every case expression a fresh identifier is introduced for the
---     scrutinized expression. For instance the expression
---
---     case e of p1 -> e1; ... ; pn -> en
---
---     is transformed to
---
---     let cid = e in case cid of p1 -> e1; ... ; pn -> en
---
---     where cid is a fresh identifier
---
---   - Furthermore, cases with boolean expressions as arguments are normalized
---     in the following form:
---
---     case c1 && c2 of                            case c1 of
---       True  -> e1         is transformed to       True  -> case c2 of
---       False -> e2                                            True  -> e1
---                                                              False -> e2
---                                                   False -> e2
---
---     Disjunctions are transformed in a similar way.
---
--- @author  Jan Tikovsky
--- @version July 2017
--- ----------------------------------------------------------------------------
module IdentifyCases where

import FiniteMap
import FlatCurry.Annotated.Types
import FlatCurry.Annotated.Goodies (annExpr, combArgs)

import FlatCurryGoodies            ( eqPattern, falsePat, isBoolType, isConj
                                   , isDisj, truePat )
import Utils

data ICM a = IC { runICM :: VarIndex -> (a, VarIndex) }

instance Monad ICM where
  return x = IC $ \s -> (x, s)

  f >>= g = IC $ \s -> let (x, s') = runICM f s in (runICM (g x)) s'

get :: ICM VarIndex
get = IC $ \s -> (s, s)

put :: VarIndex -> ICM ()
put s = IC $ \_ -> ((), s)

freshID :: ICM VarIndex
freshID = do
  v <- get
  put (v - 1)
  return v

--- Add a unique identifier to all expressions which are scrutinized in cases
idCases :: [AFuncDecl TypeExpr] -> ([AFuncDecl TypeExpr], VarIndex)
idCases fs = (runICM (mapM icFunc fs)) (-1)

icFunc :: AFuncDecl TypeExpr -> ICM (AFuncDecl TypeExpr)
icFunc (AFunc qn a vis ty r) = AFunc qn a vis ty <$> icRule r

icRule :: ARule TypeExpr -> ICM (ARule TypeExpr)
icRule (ARule  ann vs e) = ARule ann vs <$> splitCases e -- icExpr e
icRule e@(AExternal _ _) = return e

icExpr :: AExpr a -> ICM (AExpr a)
icExpr v@(AVar         _ _) = return v
icExpr l@(ALit         _ _) = return l
icExpr (AComb ann ct qn es) = AComb ann ct qn <$> mapM icExpr es
icExpr (ALet      ann bs e) = ALet ann <$> mapM icBinding bs <*> icExpr e
  where icBinding (v, ve) = icExpr ve >>= \ve' -> return (v, ve')
icExpr (AFree     ann vs e) = AFree ann vs <$> icExpr e
icExpr (AOr      ann e1 e2) = AOr ann <$> icExpr e1 <*> icExpr e2
icExpr (ACase  ann ct e bs) = do
  v <- freshID
  bs' <- mapM icBranch bs
  return $ ALet ann [((v, annE), e)] (ACase ann ct (AVar annE v) bs')
 where annE                    = annExpr e
       icBranch (ABranch p be) = ABranch p <$> icExpr be
icExpr (ATyped   ann e ty) = flip (ATyped ann) ty <$> icExpr e

splitCases :: AExpr TypeExpr -> ICM (AExpr TypeExpr)
splitCases v@(AVar        _ _) = return v
splitCases l@(ALit        _ _) = return l
splitCases (AComb ty ct qn es) = AComb ty ct qn <$> mapM splitCases es
splitCases (ALet      ty bs e) = ALet ty <$> mapM splitBinding bs
                                         <*> splitCases e
  where splitBinding (v, ve) = splitCases ve >>= \ve' -> return (v, ve')
splitCases (AFree     ty vs e) = AFree ty vs <$> splitCases e
splitCases (AOr      ty e1 e2) = AOr ty <$> splitCases e1 <*> splitCases e2
splitCases c@(ACase  ty ct e bs)
  | isBoolType (annExpr e)     = idCase (splitCase ty ct e bs)
  | otherwise                  = idCase c
splitCases (ATyped   ty e ty') = flip (ATyped ty) ty' <$> splitCases e

--- Split up case expressions over conditionals (i.e. sequences of simple
--- boolean expressions connected with `&&` or `||`)
splitCase :: TypeExpr -> CaseType -> AExpr TypeExpr -> [ABranchExpr TypeExpr]
          -> AExpr TypeExpr
splitCase ty ct e bs
  | isConj e  = splitCase ty ct c1 (branch truePat  : selBranch falsePat bs)
  | isDisj e  = splitCase ty ct c1 (branch falsePat : selBranch truePat  bs)
  | otherwise = ACase ty ct e bs
  where [c1,c2]   = combArgs e
        innerCase = splitCase ty ct c2 bs
        branch p  = ABranch p innerCase

--- Add a unique identifier to a case expression
idCase :: AExpr TypeExpr -> ICM (AExpr TypeExpr)
idCase exp = case exp of
  ACase ty ct e bs -> do
    v   <- freshID
    bs' <- mapM splitBranch bs
    return $ ALet ty [((v, tyE), e)] (ACase ty ct (AVar tyE v) bs')
   where tyE                        = annExpr e
         splitBranch (ABranch p be) = ABranch p <$> splitCases be
  _                -> splitCases exp

-- helper

-- select the branch expression matching the given pattern
selBranch :: APattern a -> [ABranchExpr a] -> [ABranchExpr a]
selBranch _ []                     = []
selBranch p (b@(ABranch q _) : bs)
  | eqPattern p q                  = [b]
  | otherwise                      = selBranch p bs
