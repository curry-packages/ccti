module Symbolic where

import FlatCurry.Annotated.Types (QName)
import FlatCurry.Types           (TypeExpr, VarIndex)

import PrettyPrint
import SMTLib.Pretty
import SMTLib.Types    (Command, Term)

data BranchNr = BNr Int Int
  deriving Show

--- Trace of visited case expressions (along with selected branch decision)
type Trace = [Decision]

type CaseID = VarIndex

data Decision = Decision CaseID BranchNr VarIndex (QName, TypeExpr) [VarIndex]
  deriving Show

--- Pretty printing
instance Pretty BranchNr where
  pretty (BNr m n) = int m <> text "/" <> int n

instance Pretty Decision where
  pretty (Decision cid bnr v (c, _) args)
    =  text "caseID" <+> ppVarIndex cid <> colon
   <+> tupledSpaced [ text "Branch" <+> pretty bnr, ppVarIndex v, ppQName c
                    , list (map ppVarIndex args)
                    ]

--- depth of a symbolic node in a symbolic execution tree
type Depth = Int

--- A symbolic node includes the following information:
---   * the depth of the node in the execution tree,
---   * the case id,
---   * possible variable declarations (SMT-LIB commands),
---   * possible path constraints (in SMT-LIB representation) and
---   * the decision variable of this node
data SymNode = SymNode
  { depth  :: Depth
  , cid    :: VarIndex
  , vdecls :: [Command]
  , pcs    :: [Term]
  , dvar   :: VarIndex
  }
 deriving Show

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
