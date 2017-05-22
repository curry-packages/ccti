--- ----------------------------------------------------------------------------
--- This module provides an abstract representation of the SMT-LIB language.
--- The implementation is based on the SMT-LIB Standard 2.5
--- (http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.5-r2015-06-28.pdf)
--- and covers most parts of the language description.
---
--- @author  Jan Tikovsky
--- @version April 2017
--- ----------------------------------------------------------------------------
module SMTLib.Types where

type Symbol  = String
type Ident   = String
type Numeral = Int

data Keyword = KW Symbol
  deriving (Eq, Show)

data SMTLib = SMTLib [Command]
  deriving (Eq, Show)

--- S-expressions

-- hexadecimal, binary missing
data SpecConstant = Num Numeral
                  | Dec Float
                  | Str String
  deriving (Eq, Show)

data SExpr = SEConst SpecConstant
           | SESym   Symbol
           | SEKW    Keyword
           | SEList  [SExpr]
  deriving (Eq, Show)

--- Sorts

data Sort = SComb Ident [Sort]
  deriving (Eq, Ord, Show)

--- Attributes

data AttrValue = AVConst SpecConstant
               | AVSym   Symbol
               | AVSExpr [SExpr]
  deriving (Eq, Show)

data Attribute = AKW  Keyword
               | AVal Keyword AttrValue
  deriving (Eq, Show)

--- Terms

data QIdent = Id Ident
            | As Ident Sort
  deriving (Eq, Show)

data SortedVar = SV Symbol Sort
  deriving (Eq, Show)

data Term = TConst SpecConstant
          | TComb QIdent [Term]
          | Let [(Symbol, Term)] Term
          | Forall [SortedVar] Term
          | Exists [SortedVar] Term
          | Annot Term [Attribute]
  deriving (Eq, Show)

--- Theories

data SortSymDecl = SortSymDecl Ident Numeral [Attribute]
  deriving (Eq, Show)

data MetaSpecConstant = NUMERAL
                      | DECIMAL
                      | STRING
  deriving (Eq, Show)

data FunSymDecl = Spec SpecConstant Sort [Attribute]
                | Meta MetaSpecConstant Sort [Attribute]
                | Ident Ident [Sort] [Attribute]
  deriving (Eq, Show)

data ParFunSymDecl = ParFunSymDecl FunSymDecl [Symbol] Ident [Sort] [Attribute]
  deriving (Eq, Show)

data TheoryAttr = Sorts [SortSymDecl]
                | Funs [ParFunSymDecl]
                | SortsDesc String
                | FunsDesc String
                | Definition String
                | Values String
                | Notes String
                | TAttr Attribute
  deriving (Eq, Show)

data Theory = Theory Symbol [TheoryAttr]
  deriving (Eq, Show)

--- Info flags

data InfoFlag = AllStatistics
              | AssertionStackLevels
              | Authors
              | ErrorBehavior
              | Name
              | ReasonUnknown
              | Version
              | IFKW Keyword
  deriving (Eq, Show)

--- Command options

data Option = DiagnosticOutput     String
            | GlobalDecls          Bool
            | Interactive          Bool
            | PrintSuccess         Bool
            | ProduceAssertions    Bool
            | ProduceAssign        Bool
            | ProduceModels        Bool
            | ProduceProofs        Bool
            | ProduceUnsatAssump   Bool
            | ProduceUnsatCores    Bool
            | RandomSeed           Numeral
            | RegularOutput        String
            | ReproducibleResLimit Numeral
            | Verbosity            Numeral
            | OptAttr              Attribute
  deriving (Eq, Show)

--- Commands

data FunDec = FunDec Symbol [SortedVar] Sort
  deriving (Eq, Show)

data FunDef = FunDef Symbol [SortedVar] Sort Term
  deriving (Eq, Show)

data PropLit = Sym Symbol
             | Not Symbol
  deriving (Eq, Show)

data ConsDecl = Cons Ident [SortedVar]
  deriving (Eq, Show)

data Command = Assert Term
             | CheckSat
             | CheckSatAssuming [PropLit]
             | DeclareConst Symbol Sort
             | DeclareDatatypes [Ident] Symbol [ConsDecl] -- currently not part of the SMTLIB standard
             | DeclareFun Symbol [Sort] Sort
             | DeclareSort Symbol Numeral
             | DefineFun FunDef
             | DefineFunRec FunDef
             | DefineFunsRec [(FunDec, Term)]
             | DefineSort Symbol [Symbol] Sort
             | Echo String
             | Exit
             | GetAssertions
             | GetAssignment
             | GetInfo InfoFlag
             | GetModel
             | GetOption Option
             | GetProof
             | GetUnsatAssumptions
             | GetUnsatCore
             | GetValue [Term]
             | Pop  Numeral
             | Push Numeral
             | Reset
             | ResetAssertions
             | SetInfo Attribute
             | SetLogic Logic
             | SetOption Option
  deriving (Eq, Show)

--- Logics provided by the SMT-LIB Standard
---
--- Explanation of the naming conventions:
---
---   * QF: restriction to quantifier free formulas
---   * A or AX: theory of ArraysEx
---   * BV: theory FixedSizeBitVectors
---   * FP: theory FloatingPoint
---   * IA: theory Ints (Integer Arithmetic)
---   * RA: theory Reals (Real Arithmetic)
---   * IRA: theory Reals_Ints (mixed Integer Real Arithmetic)
---   * IDL: Integer Difference Logic
---   * RDL: Rational Difference Logic
---   * L before IA, RA or IRA: linear fragment of those arithmetics
---   * N before IA, RA or IRA: non-linear fragment of those arithmetics
---   * UF extension allowing free sort and function symbols
---
--- see http://smtlib.cs.uiowa.edu/logics.shtml for more details
data Logic = ALL
           | AUFLIA
           | AUFLIRA
           | AUFNIRA
           | LIA
           | LRA
           | QFABV
           | QFAUFBV
           | QFAUFLIA
           | QFAX
           | QFBV
           | QFIDL
           | QFLIA
           | QFLRA
           | QFNIA
           | QFNRA
           | QFRDL
           | QFUF
           | QFUFBV
           | QFUFIDL
           | QFUFLIA
           | QFUFLRA
           | QFUFNRA
           | UFLRA
           | UFNIA
  deriving (Eq, Show)

--- Command responses

type ValuationPair  = (Term, Term)
type TValuationPair = (Symbol, Bool)

data CmdResponse = SuccessRsp
                 | UnsupportedRsp
                 | ErrorRsp               String
                 | CheckSatRsp            CheckSat
                 | EchoRsp                String
                 | GetAssertionsRsp       [Term]
                 | GetAssignmentRsp       [TValuationPair]
                 | GetInfoRsp             [InfoRsp]
                 | GetModelRsp            [ModelRsp]
                 | GetOptionRsp           AttrValue
                 | GetProofRsp            SExpr
                 | GetUnsatAssumptionsRsp [Symbol]
                 | GetUnsatCoreRsp        [Symbol]
                 | GetValueRsp            [ValuationPair]
  deriving (Eq, Show)

data CheckSat = Sat
              | Unsat
              | Unknown
  deriving (Eq, Show)

data InfoRsp = AssertionStackLevelsRsp Numeral
             | AuthorsRsp              String
             | ErrorBehaviorRsp        ErrorBehavior
             | NameRsp                 String
             | ReasonUnknownRsp        ReasonUnknown
             | VersionRsp              String
             | AttrRsp                 Attribute
  deriving (Eq, Show)

data ErrorBehavior = ImmediateExit
                   | ContinuedExecution
  deriving (Eq, Show)

data ReasonUnknown = Memout
                   | Incomplete
                   | SEReason SExpr
  deriving (Eq, Show)

data ModelRsp = MRFun     FunDef
              | MRFunRec  FunDef
              | MRFunsRec [(FunDec, Term)]
  deriving (Eq, Show)

