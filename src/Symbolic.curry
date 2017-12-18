--- ----------------------------------------------------------------------------
--- This module provides data structures for tracing symbolic information.
---
--- @author  Jan Tikovsky
--- @version December 2017
--- ----------------------------------------------------------------------------
module Symbolic where

import FlatCurry.Annotated.Pretty (ppLiteral, ppQName, ppTypeExp, ppVarIndex)
import FlatCurry.Annotated.Types
import List                       (nub)
import Text.Pretty

import FlatCurryGoodies           (boolType, charType, floatType, intType, isBoolType, prel)
import Language.SMTLIB            (Term)
import Utils                      (mapFst)

--- ----------------------------------------------------------------------------
--- Symbolic objects
--- ----------------------------------------------------------------------------

--- Symbolic object
---   * symbolic FlatCurry constructor
---   * constraint on symbolic literal
data SymObj = SymCons QName TypeExpr
            | SymLit LConstr Literal
 deriving Show

--- Create a symbolic constructor for a typed FlatCurry 'Prelude' constructor
prelSymCons :: String -> TypeExpr -> SymObj
prelSymCons c ty = SymCons (prel c) ty

--- Get the type of a symbolic object
soType :: SymObj -> TypeExpr
soType (SymCons _ ty) = ty
soType (SymLit   _ l) = case l of
  Intc   _ -> intType
  Floatc _ -> floatType
  Charc  _ -> charType

--- Literal constraint
data LConstr = E | NE | L | LE | G | GE
 deriving (Eq, Ord)

instance Show LConstr where
  show E  = "=%"
  show NE = "/=%"
  show L  = "<%"
  show LE = "<=%"
  show G  = ">%"
  show GE = ">=%"

--- Negate a literal constraint
lcNeg :: LConstr -> LConstr
lcNeg E  = NE
lcNeg NE = E
lcNeg L  = GE
lcNeg LE = G
lcNeg G  = LE
lcNeg GE = L

--- Mirror literal constraint due to swapping of argument order
lcMirror :: LConstr -> LConstr
lcMirror lc = case lc of
  L  -> G
  G  -> L
  LE -> GE
  GE -> LE
  _  -> lc

instance Pretty SymObj where
  pretty (SymCons qn ty) = ppQName qn <+> doubleColon <+> ppTypeExp ty
  pretty (SymLit  lcs l) = pretty lcs <+> ppLiteral l

instance Pretty LConstr where
  pretty E  = text "=="
  pretty NE = text "/="
  pretty L  = text "<"
  pretty LE = text "<="
  pretty G  = text ">"
  pretty GE = text ">="

--- Consider only the qualified name when comparing two typed FlatCurry
--- constructors
instance Eq SymObj where
  o1 == o2 = case (o1, o2) of
    (SymCons qn1 _ , SymCons qn2   _) -> qn1  == qn2
    (SymLit lcs1 l1, SymLit  lcs2 l2) -> lcs1 == lcs2 && l1  == l2
    _                                 -> False

--- Consider only the qualified name when comparing two typed FlatCurry
--- constructors
instance Ord SymObj where
  compare c1 c2 = case (c1, c2) of
    (SymCons qn1 _, SymCons qn2 _) -> compare qn1 qn2
    (SymLit   _ l1, SymLit   _ l2) -> compare l1  l2

--- Generate a literal constraint from the given FlatCurry expression,
--- if possible
getLConstr :: QName -> AExpr TypeExpr -> Maybe (VarIndex, SymObj)
getLConstr qn e = case (rmvApplies e) of
  AComb ty _ (fn, _) es
    | isBoolType ty -> do
      lc <- fmap (if snd qn == "False" then lcNeg else id) (lookup fn litConstrs)
      case es of
        [ALit _ l, AVar _ v] -> return (v, SymLit (lcMirror lc) l)
        [AVar _ v, ALit _ l] -> return (v, SymLit lc l)
        _                    -> Nothing
  _                          -> Nothing

rmvApplies :: AExpr TypeExpr -> AExpr TypeExpr
rmvApplies = rmvApplies' []
  where
  rmvApplies' args e = case e of
    AComb _ ct qn es
      | isApplyCall e  -> case es of
                            [f,arg] -> rmvApplies' (arg:args) f
                            _       -> error "IdentifyCases.rmvApplies"
      | not (null args) -> AComb boolType ct qn args
    _                   -> e

isApplyCall :: AExpr a -> Bool
isApplyCall e = case e of
  AComb _ FuncCall qn _ -> snd (fst qn) == "apply"
  _                     -> False

--- List of supported literal constraints
litConstrs :: [(QName, LConstr)]
litConstrs = map (mapFst prel)
  [("prim_eqInt", E), ("prim_ltEqInt", LE)]

--- ----------------------------------------------------------------------------
--- Symbolic tracing of case branches
--- ----------------------------------------------------------------------------

--- Symbolic trace
type Trace = [Decision]

--- Symbolic information for branch decisions
---   * identifier of case expression
---   * number of selected branch
---   * associated symbolic variable
---   * symbolic object (i.e. chosen constructor or literal constraint)
---   * possible symbolic variables for arguments
data Decision = Decision CaseID BranchNr VarIndex SymObj [VarIndex]
 deriving Show

--- Case identifier
type CaseID = VarIndex

--- Branch number, i.e. branch m of n branches
data BranchNr = BNr Int Int
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

--- Pretty printing of a symbolic trace
ppTrace :: Trace -> Doc
ppTrace = listSpaced . map pretty

--- Get all symbolic variables of a decision
getSVars :: Decision -> [VarIndex]
getSVars (Decision _ _ sv _ args) = sv : args

--- Rename all variables occuring in a symbolic trace
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

--- ----------------------------------------------------------------------------
--- Nodes of symbolic execution tree
--- ----------------------------------------------------------------------------

--- Representation of path constraints, i.e.
---   * decision variable
---   * symbolic object
---   * argument variables
type PathConstr = (VarIndex, SymObj, [VarIndex])

--- A symbolic node includes the following information:
---   * the depth of the node in the execution tree,
---   * the context of the node, i.e. its case id and preceeding case ids,
---   * indices of SMT-LIB constants which are required for the SMT-LIB model
---   * possible path constraints and
---   * the decision variables introduced up to this node
---   * the decision variable of this node
data SymNode = SymNode
  { depth     :: Depth
  , context   :: Context
  , constants :: [VarIndex]
  , pcs       :: [PathConstr]
  , dvars     :: [VarIndex]
  , dvar      :: VarIndex
  }
 deriving (Eq, Show)

--- Node depth in a symbolic execution tree
type Depth = Int

--- A context is a case identifier followed by a sequence of preceeding case identifiers
type Context = [VarIndex]

--- ----------------------------------------------------------------------------
--- Coverage information
--- ----------------------------------------------------------------------------

--- Coverage information for case expressions
---   * ids of uncovered branches
---   * information on already covered constructors / literal constraints
data CoverInfo = CoverInfo
  { uncovered :: [Int]
  , coveredCs :: CoveredCs
  }
  deriving Show

--- Covered constructors / literal constraints
data CoveredCs = CCons   [(SymObj, [VarIndex])]
               | CConstr LConstr Literal

--- Create information on covered constructors / constraint
mkCoveredCs :: SymObj -> [VarIndex] -> CoveredCs
mkCoveredCs sobj@(SymCons qn _) args = CCons [(sobj, args)]
mkCoveredCs (SymLit       lc l) _    = CConstr lc l

instance Show CoveredCs where
  show (CCons     cs) = show cs
  show (CConstr lc l) = show lc ++ show l
