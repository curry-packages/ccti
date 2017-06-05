module Symbolic where

import FlatCurry.Annotated.Types (QName)
import FlatCurry.Types           (TypeExpr, VarIndex)

import FlatCurryGoodies          (TypedFCYCons (..))
import PrettyPrint
import SMTLib.Pretty
import SMTLib.Types              (Command, Term)

data BranchNr = BNr Int Int
  deriving Show

--- Trace of visited case expressions (along with selected branch decision)
type Trace = [Decision]

type CaseID = VarIndex

data Decision = Decision CaseID BranchNr VarIndex TypedFCYCons [VarIndex]
  deriving Show

--- Pretty printing
instance Pretty BranchNr where
  pretty (BNr m n) = int m <> text "/" <> int n

instance Pretty Decision where
  pretty (Decision cid bnr v (TFCYCons c _) args)
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
---   * the decision variables introduced up to this node
---   * the decision variable of this node
data SymNode = SymNode
  { depth  :: Depth
  , cid    :: VarIndex
  , vdecls :: [Command]
  , pcs    :: [Term]
  , dvars  :: [VarIndex]
  , dvar   :: VarIndex
  }
 deriving Show
