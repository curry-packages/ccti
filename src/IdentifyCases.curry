--- ----------------------------------------------------------------------------
--- This module provides transforms FlatCurry programs for concolic evaluation:
---   - For every case expression a fresh identifier is introduced for the
---     expression under inspection. For instance the expression
---
---     case e of p1 -> e1; ... ; pn -> en
---
---     is transformed to
---
---     let cid = e in case cid of p1 -> e1; ... ; pn -> en
---
---     where cid is a fresh identifier
---
---   - Furthermore, information about the number of branches is collected
---     for every case expression during this transformation.
---
--- @author  Jan Tikovsky
--- @version April 2017
--- ----------------------------------------------------------------------------
module IdentifyCases where

import FiniteMap
import FlatCurry.Types

import Utils

data ICState = ICState
  { nextID   :: Int
  , branches :: FM Int [Int]
  }

initState :: ICState
initState = ICState
  { nextID   = -1
  , branches = emptyFM (<)
  }

data ICM a = IC { runICM :: ICState -> (a, ICState) }

instance Monad ICM where
  return x = IC $ \s -> (x, s)

  (IC f) >>= g = IC $ \s -> let (x, s') = f s in (runICM (g x)) s'

gets :: (ICState -> a) -> ICM a
gets f = IC $ \s -> (f s, s)

get :: ICM ICState
get = gets id

put :: ICState -> ICM ()
put s = IC $ \_ -> ((), s)

modify :: (ICState -> ICState) -> ICM ()
modify f = IC $ \s -> ((), f s)

freshID :: ICM Int
freshID = do
  v <- gets nextID
  modify (\s -> s { nextID = v - 1 })
  return v

-- TODO: remove
freshIDs :: Int -> ICM [Int]
freshIDs n = sequence $ replicate n freshID

addBranches :: Int -> Int -> ICM ()
addBranches cid bnr =
  modify (\s -> s { branches = addToFM (branches s) cid [1 .. bnr] })

--- Add a unique identifier to all expressions which are inspected in cases
--- and collect the number of branches for every case expression
idCases :: [FuncDecl] -> [FuncDecl]
idCases fs = fst $ (runICM (mapM icFunc fs)) initState

icProg :: Prog -> ICM Prog
icProg (Prog m is ts fs os) = do
  fs' <- mapM icFunc fs
  return (Prog m is ts fs' os)

icFunc :: FuncDecl -> ICM FuncDecl
icFunc (Func qn a vis ty r)
--   | snd qn == "main" = Func qn a vis ty <$> icMain r
  | otherwise        = Func qn a vis ty <$> icRule r

icRule :: Rule -> ICM Rule
icRule (Rule    vs e) = Rule vs <$> icExpr e
icRule e@(External _) = return e

icExpr :: Expr -> ICM Expr
icExpr v@(Var       _) = return v
icExpr l@(Lit       _) = return l
icExpr (Comb ct qn es) = Comb ct qn <$> mapM icExpr es
icExpr (Let      bs e) = Let <$> mapM icBinding bs <*> icExpr e
  where icBinding (v, ve) = icExpr ve >>= \ve' -> return (v, ve')
icExpr (Free     vs e) = Free vs <$> icExpr e
icExpr (Or      e1 e2) = Or <$> icExpr e1 <*> icExpr e2
icExpr (Case  ct e bs) = do
  v <- freshID
  bs' <- mapM icBranch bs
  addBranches v (length bs)
  return $ Let [(v, e)] (Case ct (Var v) bs')
 where icBranch (Branch p be) = Branch p <$> icExpr be
icExpr (Typed    e ty) = flip Typed ty <$> icExpr e

-- TODO: remove
icMain :: Rule -> ICM Rule
icMain (Rule vs e) = Rule vs <$> icMainExp e
icMain e@(External _) = return e

icMainExp e = case e of
  Comb ct qn es
    | ct == FuncCall -> do
        vs <- freshIDs (length es)
        let bs = zip vs es
        return $ Let bs $ Comb ct qn $ map Var vs
  _             -> return e
