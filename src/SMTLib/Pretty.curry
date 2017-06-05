--- ----------------------------------------------------------------------------
--- This module provides a pretty printer for the SMT-LIB language.
---
--- @author  Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module SMTLib.Pretty where

import SMTLib.Types
import PrettyPrint

instance Pretty SMTLib where
  pretty (SMTLib cmds) = vsep (map pretty cmds)

instance Pretty Keyword where
  pretty (KW sym) = colon <> text sym

instance Pretty SpecConstant where
  pretty (Num n) = int n
  pretty (Dec f) = float f
  pretty (Str s) = text s

instance Pretty SExpr where
  pretty (SEConst  c) = pretty c
  pretty (SESym  sym) = text sym
  pretty (SEKW     k) = pretty k
  pretty (SEList ses) = parent (map pretty ses)

instance Pretty Sort where
  pretty (SComb i ss) = parensIf (not $ null ss) $
                          text i <+> (hsep (map pretty ss))

instance Pretty AttrValue where
  pretty (AVConst   c) = pretty c
  pretty (AVSym   sym) = text sym
  pretty (AVSExpr ses) = parent (map pretty ses)

instance Pretty Attribute where
  pretty (AKW    k) = pretty k
  pretty (AVal k v) = pretty k <+> pretty v

instance Pretty QIdent where
  pretty (Id   i) = text i
  pretty (As i s) = parent [text "as", text i, pretty s]

instance Pretty SortedVar where
  pretty (SV sym s) = parent [text sym, pretty s]

instance Pretty Term where
  pretty (TConst     c) = pretty c
  pretty (TComb  qi ts) = parensIf (not $ null ts) $
                           pretty qi <+> (hsep (map pretty ts))
  pretty (Let     bs t) = parent [text "let", parent (map ppBind bs), pretty t]
    where ppBind (sym, t') = parent [text sym, pretty t']
  pretty (Forall svs t) = parent [ text "forall"
                                 , parent (map pretty svs)
                                 , pretty t
                                 ]
  pretty (Exists svs t) = parent [ text "exists"
                                 , parent (map pretty svs)
                                 , pretty t
                                 ]
  pretty (Annot   t as) = parent [char '!', pretty t, hsep (map pretty as)]

instance Pretty SortSymDecl where
  pretty (SortSymDecl i n as) = parent [text i, int n, hsep (map pretty as)]

instance Pretty MetaSpecConstant where
  pretty = text . show

instance Pretty FunSymDecl where
  pretty (Spec   c s as) = parent [pretty c, pretty s, hsep (map pretty as)]
  pretty (Meta   c s as) = parent [pretty c, pretty s, hsep (map pretty as)]
  pretty (Ident i ss as) = parent [ text i, hsep (map pretty ss)
                                  , hsep (map pretty as)
                                  ]

instance Pretty ParFunSymDecl where
  pretty (ParFunSymDecl f syms i ss as) = pretty f <+>
    parent [ text "par"
           , parent (map text syms)
           , parent [text i, hsep (map pretty ss), hsep (map pretty as)]]

instance Pretty TheoryAttr where
  pretty (Sorts   sdecls) = text ":sorts" <+> parent (map pretty sdecls)
  pretty (Funs    pdecls) = text ":funs" <+> parent (map pretty pdecls)
  pretty (SortsDesc  str) = text ":sorts-description" <+> text str
  pretty (FunsDesc   str) = text ":funs-description"  <+> text str
  pretty (Definition str) = text ":definition"        <+> text str
  pretty (Values     str) = text ":values"            <+> text str
  pretty (Notes      str) = text ":notes"             <+> text str
  pretty (TAttr        a) = pretty a

instance Pretty Theory where
  pretty (Theory sym tas) = parent [ text "theory"
                                   , text sym, hsep (map pretty tas)
                                   ]

instance Pretty InfoFlag where
  pretty AllStatistics        = text ":all-statistics"
  pretty AssertionStackLevels = text ":assertion-stack-levels"
  pretty Authors              = text ":authors"
  pretty ErrorBehavior        = text ":error-behavior"
  pretty Name                 = text ":name"
  pretty ReasonUnknown        = text ":reason-unknown"
  pretty Version              = text ":version"
  pretty (IFKW             k) = pretty k

instance Pretty Option where
  pretty (DiagnosticOutput     s) = text ":diagnostic-output-channel"   <+> text s
  pretty (GlobalDecls          b) = text ":global-declarations"         <+> ppBool b
  pretty (Interactive          b) = text ":interactive-mode"            <+> ppBool b
  pretty (PrintSuccess         b) = text ":print-success"               <+> ppBool b
  pretty (ProduceAssertions    b) = text ":produce-assertions"          <+> ppBool b
  pretty (ProduceAssign        b) = text ":produce-assignments"         <+> ppBool b
  pretty (ProduceModels        b) = text ":produce-models"              <+> ppBool b
  pretty (ProduceProofs        b) = text ":produce-proofs"              <+> ppBool b
  pretty (ProduceUnsatAssump   b) = text ":produce-unsat-assumptions"   <+> ppBool b
  pretty (ProduceUnsatCores    b) = text ":produce-unsat-cores"         <+> ppBool b
  pretty (RandomSeed           n) = text ":random-seed"                 <+> int n
  pretty (RegularOutput        s) = text ":regular-output-channel"      <+> text s
  pretty (ReproducibleResLimit n) = text ":reproducible-resource-limit" <+> int n
  pretty (Verbosity            n) = text ":verbosity"                   <+> int n
  pretty (OptAttr              a) = pretty a

--- Pretty printing of booleans
ppBool :: Bool -> Doc
ppBool True  = text "true"
ppBool False = text "false"

instance Pretty FunDec where
  pretty (FunDec sym svs s) = parent [ text sym, parent (map pretty svs)
                                     , pretty s
                                     ]

instance Pretty FunDef where
  pretty (FunDef sym svs s t) = parent [ text sym, parent (map pretty svs)
                                       , pretty s, pretty t
                                       ]

instance Pretty PropLit where
  pretty (Sym sym) = text sym
  pretty (Not sym) = parent [text "not", text sym]

instance Pretty Command where
  pretty = parent . ppCmd

instance Pretty ConsDecl where
  pretty (Cons i svs) = parensIf (not $ null svs) $
                          text i <+> (hsep (map pretty svs))

--- Pretty printing of SMT-LIB commands.
ppCmd :: Command -> [Doc]
ppCmd (Assert                  t) = [text "assert", pretty t]
ppCmd CheckSat                    = [text "check-sat"]
ppCmd (CheckSatAssuming       ps) = [ text "check-sat-assuming"
                                    , parent (map pretty ps)
                                    ]
ppCmd (DeclareConst        sym s) = [text "declare-const", text sym, pretty s]
ppCmd (DeclareDatatypes tys s cs) = [ text "declare-datatypes"
                                    , parent (map text tys)
                                    , parens (parent (text s : map pretty cs))
                                    ]
ppCmd (DeclareFun       sym ss s) = [ text "declare-fun"
                                    , text sym
                                    , parent (map pretty ss)
                                    , pretty s
                                    ]
ppCmd (DeclareSort         sym n) = [text "declare-sort", text sym, int n]
ppCmd (DefineFun               f) = [text "define-fun", pretty f]
ppCmd (DefineFunRec            f) = [text "define-fun-rec", pretty f]
ppCmd (DefineFunsRec         fts) = let (fs, ts) = unzip fts in
                                    [ text "define-funs-rec"
                                    , parent (map pretty fs)
                                    , parent (map pretty ts)
                                    ]
ppCmd (DefineSort     sym syms s) = [text sym, parent (map text syms), pretty s]
ppCmd (Echo                  str) = [text "echo", dquotes (text str)]
ppCmd Exit                        = [text "exit"]
ppCmd GetAssertions               = [text "get-assertions"]
ppCmd GetAssignment               = [text "get-assignment"]
ppCmd (GetInfo              flag) = [text "get-info", pretty flag]
ppCmd GetModel                    = [text "get-model"]
ppCmd (GetOption             opt) = [text "get-option", pretty opt]
ppCmd GetProof                    = [text "get-proof"]
ppCmd GetUnsatAssumptions         = [text "get-unsat-assumptions"]
ppCmd GetUnsatCore                = [text "get-unsat-core"]
ppCmd (GetValue               ts) = [text "get-value", parent (map pretty ts)]
ppCmd (Pop                   lvl) = [text "pop", int lvl]
ppCmd (Push                  lvl) = [text "push", int lvl]
ppCmd Reset                       = [text "reset"]
ppCmd ResetAssertions             = [text "reset-assertions"]
ppCmd (SetInfo                 a) = [text "set-info", pretty a]
ppCmd (SetLogic                l) = [text "set-logic", pretty l]
ppCmd (SetOption             opt) = [text "set-option", pretty opt]

instance Pretty Logic where
  pretty ALL      = text "ALL"
  pretty AUFLIA   = text "AUFLIA"
  pretty AUFLIRA  = text "AUFLIRA"
  pretty AUFNIRA  = text "AUFNIRA"
  pretty LIA      = text "LIA"
  pretty LRA      = text "LRA"
  pretty QFABV    = text "QF_ABV"
  pretty QFAUFBV  = text "QF_AUFBV"
  pretty QFAUFLIA = text "QF_AUFLIA"
  pretty QFAX     = text "QF_AX"
  pretty QFBV     = text "QF_BV"
  pretty QFIDL    = text "QF_IDL"
  pretty QFLIA    = text "QF_LIA"
  pretty QFLRA    = text "QF_LRA"
  pretty QFNIA    = text "QF_NIA"
  pretty QFNRA    = text "QF_NRA"
  pretty QFRDL    = text "QF_RDL"
  pretty QFUF     = text "QF_UF"
  pretty QFUFBV   = text "QF_UFBV"
  pretty QFUFIDL  = text "QF_UFIDL"
  pretty QFUFLIA  = text "QF_UFLIA"
  pretty QFUFLRA  = text "QF_UFLRA"
  pretty QFUFNRA  = text "QF_UFNRA"
  pretty UFLRA    = text "UFLRA"
  pretty UFNIA    = text "UFNIA"

instance Pretty CmdResponse where
  pretty SuccessRsp                    = text "success"
  pretty UnsupportedRsp                = text "unsupported"
  pretty (ErrorRsp                str) = parens (text "error"
                                           <+> dquotes (text str))
  pretty (CheckSatRsp              cs) = pretty cs
  pretty (EchoRsp                 str) = text str
  pretty (GetAssertionsRsp         ts) = parent (map pretty ts)
  pretty (GetAssignmentRsp        tvs) = parent (map ppTValPair tvs)
    where ppTValPair (sym, b) = parent [text sym, ppBool b]
  pretty (GetInfoRsp               is) = parent (map pretty is)
  pretty (GetModelRsp              ms) = parent (map pretty ms)
  pretty (GetOptionRsp             av) = pretty av
  pretty (GetProofRsp              se) = pretty se
  pretty (GetUnsatAssumptionsRsp syms) = parent (map text syms)
  pretty (GetUnsatCoreRsp        syms) = parent (map text syms)
  pretty (GetValueRsp              vs) = parent (map ppValPair vs)

ppValPair :: ValuationPair -> Doc
ppValPair (t1, t2) = parent [pretty t1, pretty t2]

instance Pretty CheckSat where
  pretty Sat     = text "sat"
  pretty Unsat   = text "unsat"
  pretty Unknown = text "unknown"

instance Pretty InfoRsp where
  pretty (AssertionStackLevelsRsp n) = text ":assertion-stack-levels" <+> int n
  pretty (AuthorsRsp            str) = text ":authors" <+> text str
  pretty (ErrorBehaviorRsp       eb) = text ":error-behavior" <+> pretty eb
  pretty (NameRsp               str) = text ":name" <+> text str
  pretty (ReasonUnknownRsp       ru) = text ":reason-unknown" <+> pretty ru
  pretty (VersionRsp            str) = text ":version" <+> text str
  pretty (AttrRsp                 a) = pretty a

instance Pretty ErrorBehavior where
  pretty ImmediateExit      = text "immediate-exit"
  pretty ContinuedExecution = text "continued-execution"

instance Pretty ReasonUnknown where
  pretty Memout        = text "memout"
  pretty Incomplete    = text "incomplete"
  pretty (SEReason se) = pretty se

instance Pretty ModelRsp where
  pretty (MRFun       f) = parent [text "define-fun", pretty f]
  pretty (MRFunRec    f) = parent [text "define-fun-rec", pretty f]
  pretty (MRFunsRec fts) = let (fs, ts) = unzip fts
                           in parent [ text "define-funs-rec"
                                     , parent (map pretty fs)
                                     , parent (map pretty ts)
                                     ]
