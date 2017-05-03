module Symbolic where

import FlatCurry.Types (VarIndex)

import PrettyPrint

data NthOfM = Int :/: Int
  deriving (Eq, Show)

instance Ord NthOfM where
  compare (n1 :/: m1) (n2 :/: m2) = case compare m1 m2 of
    LT -> LT
    GT -> GT
    EQ -> compare n1 n2

type BranchNr = NthOfM
type ConsNr   = NthOfM

--- Trace of visited case expressions (along with selected branch decision)
type Trace = [Decision]

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

-- type PathConstr = (BranchNr, VarIndex, ConsNr)

type Queue a = [a]



data PathConstr = Equal VarIndex ConsNr
                | Diff  VarIndex ConsNr
  deriving Show

(:=:) :: VarIndex -> ConsNr -> PathConstr
(:=:) = Equal

(:/=:) :: VarIndex -> ConsNr -> PathConstr
(:/=:) = Diff

type Branch = (BranchNr, PathConstr, SymTree)

data SymTree = Node VarIndex [Branch]
             | Leaf
  deriving Show

--- Select the path constraint of a given branch
getPC :: Branch -> PathConstr
getPC (_, pc, _) = pc

--- Negate the given path constraint
negPC :: PathConstr -> PathConstr
negPC (Equal v c) = v :/=: c
negPC (Diff  v c) = v :=:  c

--- Add the given trace of symbolic information to a symbolic tree
addTrace :: Trace -> SymTree -> SymTree
addTrace tr Leaf                 = symTree tr
addTrace tr tree@(Node cid1 cs1) = case tr of
  []               -> tree
  (cid2 :>: (SymInfo bnr v cnr _)):ds
    | cid1 == cid2 -> Node cid1 $ addBranch bnr (v :=: cnr) ds cs1
    | otherwise    -> error "Symbolic.addTrace: current trace and symbolic tree do not match"

addBranch :: BranchNr -> PathConstr -> Trace -> [Branch] -> [Branch]
addBranch bnr1 pc tr []                = [(bnr1, pc, symTree tr)]
addBranch bnr1 pc tr (b@(bnr2, _, t):bs) = case compare bnr1 bnr2 of
  LT -> (bnr1, pc, symTree    tr) : b  : bs
  EQ -> (bnr1, pc, addTrace tr t) : bs
  GT -> b : addBranch bnr1 pc tr bs

--- Transform a trace of symbolic information into a symbolic tree
symTree :: Trace -> SymTree
symTree tr = case tr of
  []                                 -> Leaf
  (cid :>: (SymInfo bnr v cnr _)):ds -> Node cid [(bnr, v :=: cnr, symTree ds)]

-- tr1 = [1 :>: (SymInfo (1 :/: 2) 707 (1 :/: 2) [])]
-- tr2 = [1 :>: (SymInfo (2 :/: 2) 707 (2 :/: 2) []), 2 :>: (SymInfo (1 :/: 2) 708 (1 :/: 2) [])]
-- tr3 = [1 :>: (SymInfo (2 :/: 2) 707 (2 :/: 2) []), 2 :>: (SymInfo (2 :/: 2) 708 (2 :/: 2) []), 1 :>: (SymInfo (1 :/: 2) 710 (1 :/: 2) [])]
