module Symbolic where

import FlatCurry.Types (VarIndex)

import PrettyPrint
import SMTLib.Pretty
import SMTLib.Types    (Term)

data BranchNr = BNr Int Int
  deriving Show

--- Trace of visited case expressions (along with selected branch decision)
type Trace = [Decision]

data Decision = VarIndex :>: SymInfo
  deriving Show

--- Get the symbolic information of a decision
symInfo :: Decision -> SymInfo
symInfo (_ :>: si) = si

data SymInfo = SymInfo BranchNr VarIndex Term
  deriving Show

--- Pretty printing
instance Pretty BranchNr where
  pretty (BNr m n) = int m <> text "/" <> int n

instance Pretty Decision where
  pretty (vi :>: info) = text "caseID" <+> ppVarIndex vi <> colon <+> pretty info

instance Pretty SymInfo where
  pretty (SymInfo bnr v t)
    = tupledSpaced [text "Branch" <+> pretty bnr, ppVarIndex v, pretty t]

-- data PathConstr = Equal VarIndex ConsNr
--                 | Diff  VarIndex ConsNr
--   deriving Show
--
-- (:=:) :: VarIndex -> ConsNr -> PathConstr
-- (:=:) = Equal
--
-- (:/=:) :: VarIndex -> ConsNr -> PathConstr
-- (:/=:) = Diff
--
-- type Branch = (BranchNr, PathConstr, SymTree)
--
-- data SymTree = Node VarIndex [Branch]
--              | Leaf
--   deriving Show
--
-- --- Select the path constraint of a given branch
-- getPC :: Branch -> PathConstr
-- getPC (_, pc, _) = pc
--
-- --- Negate the given path constraint
-- negPC :: PathConstr -> PathConstr
-- negPC (Equal v c) = v :/=: c
-- negPC (Diff  v c) = v :=:  c
--
-- --- Add the given trace of symbolic information to a symbolic tree
-- addTrace :: Trace -> SymTree -> SymTree
-- addTrace tr Leaf                 = symTree tr
-- addTrace tr tree@(Node cid1 cs1) = case tr of
--   []               -> tree
--   (cid2 :>: (SymInfo bnr v cnr _)):ds
--     | cid1 == cid2 -> Node cid1 $ addBranch bnr (v :=: cnr) ds cs1
--     | otherwise    -> error "Symbolic.addTrace: current trace and symbolic tree do not match"
--
-- addBranch :: BranchNr -> PathConstr -> Trace -> [Branch] -> [Branch]
-- addBranch bnr1 pc tr []                = [(bnr1, pc, symTree tr)]
-- addBranch bnr1 pc tr (b@(bnr2, _, t):bs) = case compare bnr1 bnr2 of
--   LT -> (bnr1, pc, symTree    tr) : b  : bs
--   EQ -> (bnr1, pc, addTrace tr t) : bs
--   GT -> b : addBranch bnr1 pc tr bs
--
-- --- Transform a trace of symbolic information into a symbolic tree
-- symTree :: Trace -> SymTree
-- symTree tr = case tr of
--   []                                 -> Leaf
--   (cid :>: (SymInfo bnr v cnr _)):ds -> Node cid [(bnr, v :=: cnr, symTree ds)]

-- tr1 = [1 :>: (SymInfo (1 :/: 2) 707 (1 :/: 2) [])]
-- tr2 = [1 :>: (SymInfo (2 :/: 2) 707 (2 :/: 2) []), 2 :>: (SymInfo (1 :/: 2) 708 (1 :/: 2) [])]
-- tr3 = [1 :>: (SymInfo (2 :/: 2) 707 (2 :/: 2) []), 2 :>: (SymInfo (2 :/: 2) 708 (2 :/: 2) []), 1 :>: (SymInfo (1 :/: 2) 710 (1 :/: 2) [])]
