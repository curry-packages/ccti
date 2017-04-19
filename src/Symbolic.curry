module Symbolic where

-- import FiniteMap
import FlatCurry.Types (VarIndex)

import PrettyPrint

data NthOfM = Int :/: Int
  deriving Show

type BranchNr = NthOfM
type ConsNr   = NthOfM

--- Trace of visited case expressions (along with selected branch decision)
type Trace = [Decision]

--- Store unvisited branches of a case expression
-- type UnvisitedBs = FM VarIndex [Int]

--- Visit branch `bid` of case expression `cid`
-- visit :: VarIndex -> Int -> Int -> Trace -> Trace
-- visit cid bid bnr trace = case lookupFM trace cid of
--   Nothing -> addToFM trace cid (delete bid [1 .. bnr])
--   Just _  -> updFM   trace cid (delete bid)

data Decision = VarIndex :>: SymInfo
  deriving Show

data SymInfo = SymInfo BranchNr VarIndex ConsNr [VarIndex]
  deriving Show

--- Pretty printing
instance Pretty NthOfM where
  pretty (m :/: n) = int m <> text "/" <> int n

instance Pretty Decision where
  pretty (vi :>: info) = text "caseID" <+> ppVarIndex vi <> colon <+> pretty info

instance Pretty SymInfo where
  pretty (SymInfo bnr v cnr vs)
    = tupledSpaced [ text "Branch" <+> pretty bnr
                   , ppVarIndex v
                   , text "Constructor" <+> pretty cnr
                   , list (map ppVarIndex vs)
                   ]
  
-- data SymTree = Node VarIndex [(PathConstr, SymTree)]
--              | Leaf
