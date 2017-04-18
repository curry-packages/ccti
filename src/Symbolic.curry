module Symbolic where

-- import FiniteMap
import FlatCurry.Types (VarIndex)

--- Trace of visited case expressions (along with selected branch decision)
type Trace = [Decision]

--- Branch m of n branches - represented by (m, n)
type BranchNr = (Int, Int)

type ConNr = (Int, Int)

--- Store unvisited branches of a case expression
-- type UnvisitedBs = FM VarIndex [Int]

--- Visit branch `bid` of case expression `cid`
-- visit :: VarIndex -> Int -> Int -> Trace -> Trace
-- visit cid bid bnr trace = case lookupFM trace cid of
--   Nothing -> addToFM trace cid (delete bid [1 .. bnr])
--   Just _  -> updFM   trace cid (delete bid)

data Decision = VarIndex :>: SymInfo
  deriving Show

data SymInfo = SymInfo BranchNr VarIndex ConNr [VarIndex]
  deriving Show

-- data SymTree = Node VarIndex [(PathConstr, SymTree)]
--              | Leaf
