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
--- @author  Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module IdentifyCases where

import FiniteMap
import FlatCurry.Annotated.Types
import FlatCurry.Annotated.Goodies (annExpr)

import Utils

data ICM a = IC { runICM :: VarIndex -> (a, VarIndex) }

instance Monad ICM where
  return x = IC $ \s -> (x, s)

  (IC f) >>= g = IC $ \s -> let (x, s') = f s in (runICM (g x)) s'

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
idCases :: [AFuncDecl a] -> ([AFuncDecl a], VarIndex)
idCases fs = (runICM (mapM icFunc fs)) (-1)

icFunc :: AFuncDecl a -> ICM (AFuncDecl a)
icFunc (AFunc qn a vis ty r) = AFunc qn a vis ty <$> icRule r

icRule :: ARule a -> ICM (ARule a)
icRule (ARule  ann vs e) = ARule ann vs <$> icExpr e
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
