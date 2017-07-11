module Symbolic where

import FlatCurry.Annotated.Types (QName)
import FlatCurry.Types           (TypeExpr, VarIndex)
import List                      (nub)

import FlatCurryGoodies          (SymObj)
import PrettyPrint
import SMTLib.Pretty
import SMTLib.Types              (Command, Term)

data BranchNr = BNr Int Int
  deriving Show

--- Trace of visited case expressions (along with selected branch decision)
type Trace = [Decision]

type CaseID = VarIndex

data Decision = Decision CaseID BranchNr VarIndex SymObj [VarIndex]
  deriving Show

--- Pretty printing
instance Pretty BranchNr where
  pretty (BNr m n) = int m <> text "/" <> int n

instance Pretty Decision where
  pretty (Decision cid bnr v sobj args)
    =  text "caseID" <+> ppVarIndex cid <> colon
   <+> tupledSpaced [ text "Branch" <+> pretty bnr, ppVarIndex v, pretty sobj
                    , list (map ppVarIndex args)
                    ]

ppTrace :: Trace -> Doc
ppTrace = listSpaced . map pretty

--- depth of a symbolic node in a symbolic execution tree
type Depth = Int

--- A symbolic node includes the following information:
---   * the depth of the node in the execution tree,
---   * the case id,
---   * indices of SMT-LIB constants which are required for the SMT-LIB model
---   * possible path constraints (in SMT-LIB representation) and
---   * the decision variables introduced up to this node
---   * the decision variable of this node
data SymNode = SymNode
  { depth     :: Depth
  , cid       :: VarIndex
  , constants :: [VarIndex]
  , pcs       :: [Term]
  , dvars     :: [VarIndex]
  , dvar      :: VarIndex
  }
 deriving (Eq, Show)

--- Get all symbolic variables of a decision
getSVars :: Decision -> [VarIndex]
getSVars (Decision _ _ sv _ args) = sv : args

--- Rename all variables occuring in a trace
rnmTrace :: [VarIndex] -> Trace -> ([Trace],VarIndex) -> ([Trace],VarIndex)
rnmTrace sargs ds (ts, v) =
  let svars = filter (`notElem` sargs) $ nub $ concatMap getSVars ds
      sub   = zip svars [v, v-1 ..]
  in (map (appSub sub) ds : ts, v - length svars)
  where
  appSub sub' (Decision cid bnr sv scon args) =
    let repl x = case lookup x sub' of
                   Nothing -> x
                   Just  y -> y
    in Decision cid bnr (repl sv) scon (map repl args)
