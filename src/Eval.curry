--- ----------------------------------------------------------------------------
--- This module provides operations to evaluate `FlatCurry` programs to
--- (head) normal form based on the natural semantics.
---
--- @author  Jan Tikovsky
--- @version April 2017
--- ----------------------------------------------------------------------------
module Eval where

import FlatCurry.Goodies
import FlatCurry.Types
import List               ((\\), find, intersect, nub)

import CCTOptions         (CCTOpts (..))
import EnumEnv            (EnumEnv, lookupEnum)
import FlatCurryGoodies
import Heap
import Output             (traceDebug)
import PrettyPrint hiding (combine)
import Substitution
import Symbolic
import Utils

import Debug
import FiniteMap

infixl 3 <|>

-- -----------------------------------------------------------------------------
-- Concolic evaluation monad
-- -----------------------------------------------------------------------------

--- Non-Deterministic state monad.
type State s a = s -> Result (a, s)

--- Non-Deterministic result
--- @cons Return - single result
--- @cons Choice - non-deterministic choice
data Result a
  = Return a
  | Choice (Result a) (Result a)

--- Internal state for concolic evaluation, consisting of
---   * the current ccti options,
---   * the concrete heap containing variable bindings,
---   * a list of all defined function declarations,
---   * an index for fresh variables,
---   * a trace of symbolic information for case branches.
data CEState = CEState
  { cesCCTOpts :: CCTOpts
  , cesHeap    :: Heap
  , cesFuncs   :: [FuncDecl]
  , cesEnumEnv :: EnumEnv
  , cesFresh   :: VarIndex
  , cesTrace   :: Trace
  }

--- Initial state for concolic evaluation
initState :: CCTOpts -> EnumEnv -> [FuncDecl] -> VarIndex -> CEState
initState opts eenv fs v = CEState
  { cesCCTOpts = opts
  , cesHeap    = emptyH
  , cesFuncs   = fs
  , cesEnumEnv = eenv
  , cesFresh   = v
  , cesTrace   = []
  }

traceStep :: Expr -> CEM a -> CEM a
traceStep e x = do
  opts <- getOpts
  h    <- getHeap
  t    <- getTrace
  traceDebug opts
    (pPrint $ vsep [ text "Heap:"           <+> ppHeap h
                   , text "Symbolic Trace:" <+> listSpaced (map pretty t)
                   , text "Expression:"     <+> ppExp defaultOptions e
                   ])
    x

--- Concolic evaluation monad
data CEM a = CE { runCEM :: CEState -> Result (a, CEState) }

runState :: CEM a -> CEState -> Result (a, CEState)
runState (CE f) s = f s

--- Monadic bind operation.
bindResult :: Result a -> (a -> Result b) -> Result b
bindResult (Return   x) f = f x
bindResult (Choice a b) f = Choice (bindResult a f) (bindResult b f)

instance Monad CEM where
  return x = CE $ \s -> Return (x, s)

  (CE f) >>= g = CE $ \s -> bindResult (f s) (\(x, s') -> runState (g x) s')

choice :: CEM a -> CEM a -> CEM a
choice x y = CE $ \s -> Choice (runState x s) (runState y s)

gets :: (CEState -> a) -> CEM a
gets f = CE $ \s -> Return (f s, s)

get :: CEM CEState
get = gets id

put :: CEState -> CEM ()
put s = CE $ \_ -> Return ((), s)

modify :: (CEState -> CEState) -> CEM ()
modify f = CE $ \s -> Return ((), f s)

--- Infix version of non-deterministic choice.
(<|>) :: CEM a -> CEM a -> CEM a
(<|>) = choice

--- ----------------------------------------------------------------------------

--- Get the ccti options
getOpts :: CEM CCTOpts
getOpts = gets cesCCTOpts

--- ----------------------------------------------------------------------------
--- Heap operations
--- ----------------------------------------------------------------------------

--- Get the current heap
getHeap :: CEM Heap
getHeap = gets cesHeap

--- Modify the current heap
modifyHeap :: (Heap -> Heap) -> CEM ()
modifyHeap f = modify $ \s -> s { cesHeap = f (cesHeap s) }

--- Lookup the binding for a variable in the current heap
lookupBinding :: VarIndex -> CEM (Maybe Binding)
lookupBinding v = lookupH v <$> getHeap

--- Bind a variable as a "black hole"
bindBH :: VarIndex -> CEM ()
bindBH v = modifyHeap (bindHole v)

--- Bind a variable to an expression
bindE :: VarIndex -> Expr -> CEM ()
bindE v e = modifyHeap (bindExpr v e)

--- Bind a variable lazily to an expression
bindLE :: VarIndex -> Expr -> CEM ()
bindLE v e = modifyHeap (bindLazyExpr v e)

--- Bind a variable as "free"
bindF :: VarIndex -> CEM ()
bindF v = modifyHeap (bindFree v)

--- Bind a variable lazily as "free"
bindLF :: VarIndex -> CEM ()
bindLF v = modifyHeap (bindLazyFree v)

--- Bind an argument of a constructor or function call
--- For let expressions and declarations of free variables the corresponding
--- bindings are directly performed in the heap and thus preventing
--- nested let expressions in the heap.
bindArg :: Expr -> CEM Expr
bindArg e = case e of
  Var  _     -> return e
  Let  bs e' -> addBindings bs e' >>= bindArg
  Free vs e' -> addFrees    vs e' >>= bindArg
  _          -> do v <- freshVar
                   bindE v e
                   return (Var v)

--- ----------------------------------------------------------------------------

--- Lookup the parameters and the rhs for a given function
lookupRule :: QName -> CEM (Maybe ([VarIndex], Expr))
lookupRule f = do
  mf <- gets (find (hasName f) . cesFuncs)
  return $ case mf of
    Just (Func _ _ _ _ (Rule vs rhs)) -> Just (vs, rhs)
    _                                 -> Nothing

--- ----------------------------------------------------------------------------

--- Get the next fresh variable
freshVar :: CEM VarIndex
freshVar = do
  s <- get
  let v = cesFresh s
  put s { cesFresh = v - 1 }
  return v

--- Get n fresh variables
freshVars :: Int -> CEM [VarIndex]
freshVars n = sequence $ replicate n freshVar

--- ----------------------------------------------------------------------------

--- Get the trace of symbolic information
getTrace :: CEM Trace
getTrace = gets cesTrace

enum :: QName -> CEM ConsNr
enum qn = do
  eenv <- gets cesEnumEnv
  case lookupEnum qn eenv of
    Just cnr -> return cnr
    Nothing  -> error $ "Eval.enumerate: Undefined constructor " ++ show qn

--- Add a decision with symbolic information for case expression `cid` to the trace
mkDecision :: VarIndex -> SymInfo -> CEM ()
mkDecision cid info = modify (\s -> s { cesTrace = (cid :>: info) : cesTrace s })

--- ----------------------------------------------------------------------------

--- concolic evaluation
ceval :: CCTOpts -> EnumEnv -> [FuncDecl] -> Expr -> Expr
ceval opts eenv fs e = fromResult $ runState (nf e) (initState opts eenv fs (-10))

--- Evaluate
-- eval :: [FuncDecl] -> Expr -> Expr
-- eval fs e = fromResult $ runState (nf e) (initState fs (-1))

fromResult :: Result (Expr, CEState) -> Expr
fromResult (Return (e, _)) = e
fromResult (Choice e1 e2)  = mkOr (fromResult e1) (fromResult e2)

mkOr :: Expr -> Expr -> Expr
mkOr e1 e2 | e1 == failedExpr = e2
           | e2 == failedExpr = e1
           | otherwise        = Or e1 e2

--- Evaluate given FlatCurry expression to normal form
nf :: Expr -> CEM Expr
nf e = hnf e >>= \e' -> case e' of
  Comb ConsCall c es -> Comb ConsCall c <$> mapM nf es
  _                  -> return e'

--- Evaluate given FlatCurry expression to head normal form
hnf :: Expr -> CEM Expr
hnf exp = case exp of
  Var        v -> traceStep exp $ hnfVar  v
  Lit        l -> traceStep exp $ hnfLit  l
  Comb ct f es -> traceStep exp $ hnfComb ct f es
  Let     bs e -> traceStep exp $ hnfLet  bs e
  Free    vs e -> traceStep exp $ hnfFree vs e
  Or     e1 e2 -> traceStep exp $ hnfOr   e1 e2
  Case ct e bs -> traceStep exp $ hnfCase ct e bs
  Typed    e _ -> traceStep exp $ hnf     e

--- Concolic evaluation of a variable
hnfVar :: VarIndex -> CEM Expr
hnfVar i = lookupBinding i >>= \mbdg -> case mbdg of
  Nothing            -> failS
  Just BlackHole     -> failS
  Just (BoundVar  e) -> bindBH i >> hnf e >>= \v -> bindE i v >> return v
  Just (LazyBound e) -> bindBH i >> hnf e >>= bindAndCheckLazy
  Just FreeVar       -> return (Var i)
  Just LazyFree      -> return (Var i)
 where
  bindAndCheckLazy v = case v of
    -- variable
    Var w -> bindLE i v >> lookupBinding w >>= \mbdg -> case mbdg of
      Nothing           -> failS
      Just BlackHole    -> failS
      Just (BoundVar e) -> bindLE w e >> return v
      Just FreeVar      -> bindLF w   >> return v
      _                 -> return v
    -- literal
    Lit _               -> bindE i v >> return v
    Comb ConsCall qn xs -> do ys <- freshVars (length xs)
                              let val = Comb ConsCall qn (map Var ys)
                              zipWithM_ bindLE ys xs
                              bindE i val
                              return val
    _                   -> error $ "Eval.bindAndCheckLazy: " ++ show v

--- Concolic evaluation of a literal
hnfLit :: Literal -> CEM Expr
hnfLit l = return (Lit l)

--- Concolic evaluation of a combination:
---   * flattened function calls: check if function is builtin or user-defined
---                               and start corresponding concolic evaluation
---   * non-flattened function/constructor calls: flatten them
hnfComb :: CombType -> QName -> [Expr] -> CEM Expr
hnfComb ct f es = case ct of
  FuncCall
    | all isVar es -> lookupRule f >>= \mrule -> case mrule of
      Nothing      -> ceBuiltin f es
      Just (xs, e) -> {-trace ((show f) ++ " " ++ show xs ++ " = " ++ show e) $-} hnf (subst (mkSubst xs es) e)
    | otherwise    -> mvs >>= \vs -> hnf (Comb ct f vs)
  _                -> Comb ct f <$> mvs
 where
  mvs = mapM bindArg es

--- Concolic evaluation of a let expression
hnfLet :: [(VarIndex, Expr)] -> Expr -> CEM Expr
hnfLet bs e = addBindings bs e >>= hnf

--- Add a list of local bindings to the heap
addBindings :: [(VarIndex, Expr)] -> Expr -> CEM Expr
addBindings bs e = do
  ys <- freshVars (length bs)
  let (xs, es) = unzip bs
      sigma    = mkSubst xs (map Var ys)
  zipWithM_ bindE ys (map (subst sigma) es)
  return (subst sigma e)

--- Concolic evaluation of a declaration of free variables
hnfFree :: [VarIndex] -> Expr -> CEM Expr
hnfFree vs e = addFrees vs e >>= hnf

--- Add a list of free variables to the heap
addFrees :: [VarIndex] -> Expr -> CEM Expr
addFrees vs e = do
  ys <- freshVars (length vs)
  mapM_ bindF ys
  return $ subst (mkSubst vs (map Var ys)) e

--- Concolic evaluation of a non-deterministic choice
hnfOr :: Expr -> Expr -> CEM Expr
hnfOr e1 e2 = hnf e1 <|> hnf e2

hnfCase :: CaseType -> Expr -> [BranchExpr] -> CEM Expr
hnfCase ct e bs = do
  (cid, e') <- rmvCaseID e
  (Var  vi) <- bindArg e' -- flattening of scrutinized expression
  hnf (Var vi) >>= \v -> case v of
    Lit l -> case findBranch (LPattern l) bs of
      Nothing          -> failS
      Just (n, _,  be) -> hnf be
    Comb ConsCall c es -> case findBranch (Pattern c []) bs of
      Nothing          -> failS
      Just (n, vs, be) -> do
        cnr <- enum c
        mkDecision cid (SymInfo (n :/: length bs) vi cnr (concatMap getVarIdx es))
        hnf (subst (mkSubst vs es) be)
    Comb FuncCall _ _
      | v == failedExpr -> failS
    Var i
      | ct == Rigid -> error "Suspended"
      | otherwise   -> narrowCase
      where
        narrowCase = foldr choice failS $ map guess bs

        guess (Branch (LPattern   l) be) = bindE i (Lit l) >> hnf be
        guess (Branch (Pattern c xs) be) = do
          ys <- freshVars (length xs)
          let es' = map Var ys
          bindE i (Comb ConsCall c es')
          hnf (subst (mkSubst xs es') be)
    _ -> error $ "Eval.hnfCase: " ++ show v

--- Remove the case identifier from the scrutinized expression of a case expression
rmvCaseID :: Expr -> CEM (VarIndex, Expr)
rmvCaseID e = case e of
  Var cid -> lookupBinding cid >>= \mbdg -> case mbdg of
               Just (BoundVar be) -> return (cid, be)
               _                  -> error $
                 "Eval.rmvCaseID: Unexpected binding " ++ show mbdg
  _       -> error $ "Eval.rmvCaseID: Expected case id but found " ++ show e

--- Concolic evaluation of a case expression
-- hnfCase :: CaseType -> Expr -> [BranchExpr] -> CEM Expr
-- hnfCase ct e bs = hnf e >>= \v -> case v of
--   Lit l -> case findBranch (LPattern l) bs of
--     Nothing       -> failS
--     Just (_,  be) -> hnf be
--   Comb ConsCall c es -> case findBranch (Pattern c []) bs of
--     Nothing       -> failS
--     Just (vs, be) -> hnf (subst (mkSubst vs es) be)
--   Comb FuncCall _ _
--     | v == failedExpr -> failS
--   Var i
--     | ct == Rigid -> error "Suspended"
--     | otherwise   -> narrowCase
--     where
--       narrowCase = foldr choice failS $ map guess bs
--
--       guess (Branch (LPattern   l) be) = bindE i (Lit l) >> hnf be
--       guess (Branch (Pattern c xs) be) = do
--         ys <- freshVars (length xs)
--         let es' = map Var ys
--         bindE i (Comb ConsCall c es')
--         hnf (subst (mkSubst xs es') be)
--   _ -> error $ "Eval.hnfCase: " ++ show v

--- Failing evaluation
failS :: CEM Expr
failS = return failedExpr

--- Evaluation to `True`
succeedS :: CEM Expr
succeedS = return trueExpr

--- Evaluation which yields a free variable
unknownS :: CEM Expr
unknownS = do
  v <- freshVar
  bindF v
  return (Var v)

-- -----------------------------------------------------------------------------
-- Concolic evaluation of builtin functions
-- -----------------------------------------------------------------------------

ceBuiltin :: QName -> [Expr] -> CEM Expr
ceBuiltin f es = case snd f of
  "apply"            -> binary ceBuiltinApply             es
  "cond"             -> binary ceBuiltinCond              es
  "ensureNotFree"    -> unary  ceBuiltinEnsureNotFree     es
  "failed"           -> ceBuiltinFailed                   es
  "prim_error"       -> unary  ceBuiltinError             es
  "success"          -> ceBuiltinSuccess                  es
  "unknown"          -> ceBuiltinUnknown                  es
  "?"                -> binary ceBuiltinChoice            es
  "&"                -> binary ceBuiltinAmp               es
  "&>"               -> binary ceBuiltinCond              es
  "&&"               -> binary ceBuiltinAnd               es
  "||"               -> binary ceBuiltinOr                es
  "$!"               -> binary (ceBuiltinDollarBangs hnf) es
  "$!!"              -> binary (ceBuiltinDollarBangs  nf) es
  "$##"              -> binary ceBuiltinDollarHashHash    es
  "=:="              -> binary ceBuiltinUni               es
  "=:<="             -> binary ceBuiltinLazyUni           es

  -- arithmetic on integers
  "prim_Int_plus"    -> binary (ceBuiltinIntOp       (+)) es
  "prim_Int_minus"   -> binary (ceBuiltinIntOp       (-)) es
  "prim_Int_times"   -> binary (ceBuiltinIntOp       (*)) es
  "prim_Int_div"     -> binary (ceBuiltinIntOp       div) es
  "prim_Int_mod"     -> binary (ceBuiltinIntOp       mod) es
  "prim_Int_quot"    -> binary (ceBuiltinIntOp      quot) es
  "prim_Int_rem"     -> binary (ceBuiltinIntOp       rem) es
  "prim_i2f"         -> unary  ceBuiltinI2F               es

  -- comparison on integers
  "prim_eqInt"       -> binary (ceBuiltinIntOp      (==)) es
  "prim_ltEqInt"     -> binary (ceBuiltinIntOp      (<=)) es

  -- conversion from and to characters
  "prim_ord"         -> unary ceBuiltinOrd                es
  "prim_chr"         -> unary ceBuiltinChr                es

  -- comparison on characters
  "prim_eqChar"      -> binary (ceBuiltinCharOp     (==)) es
  "prim_ltEqChar"    -> binary (ceBuiltinCharOp     (<=)) es

  -- arithmetic on floats
  "prim_Float_plus"  -> binary (ceBuiltinFloatOp     (+)) es
  "prim_Float_minus" -> binary (ceBuiltinFloatOp     (-)) es
  "prim_Float_times" -> binary (ceBuiltinFloatOp     (*)) es
  "prim_Float_div"   -> binary (ceBuiltinFloatOp     (/)) es
  "prim_negateFloat" -> unary  ceBuiltinNegFloat          es

  -- comparison on floats
  "prim_eqFloat"     -> binary (ceBuiltinFloatOp    (==)) es
  "prim_ltEqFloat"   -> binary (ceBuiltinFloatOp    (<=)) es
  _                  -> error $ "ceBuiltin: Unknown built in function " ++ show f

unary :: (Expr -> CEM Expr) -> [Expr] -> CEM Expr
unary f es = case es of
  [e] -> f e
  _   -> error "Eval unary"

binary :: (Expr -> Expr -> CEM Expr) -> [Expr] -> CEM Expr
binary f es = case es of
  [e1,e2] -> f e1 e2
  _       -> error "Eval.binary"

--- Concolic evaluation of a higher order application
ceBuiltinApply :: Expr -> Expr -> CEM Expr
ceBuiltinApply e1 e2 = hnf e1 >>= \v1 -> case v1 of
  Comb ct f es | isPartCall ct -> hnf $ addPartCallArg ct f es e2
  _                            -> ceBuiltinApply v1 e2

--- Concolic evaluation of a conditional expression `(&>)` / `cond`
ceBuiltinCond :: Expr -> Expr -> CEM Expr
ceBuiltinCond e1 e2 = hnf e1 >>= \v1 -> case v1 of
  Comb FuncCall g [x ,y]
      -- replace `(e1 &> e2) &> e3` by `e1 &> (e2 &> e3)`
    | snd g == "&>"    -> hnf $ builtin "&>" [x, builtin "&>" [y, e2]]
    | snd g == "cond"  -> hnf $ builtin "cond" [x, builtin "cond" [y, e2]]
  _ | v1 == trueExpr   -> hnf e2
    | v1 == failedExpr -> failS
    | otherwise        -> ceBuiltinCond v1 e2

--- Concolic evaluation of `ensureNotFree`
ceBuiltinEnsureNotFree :: Expr -> CEM Expr
ceBuiltinEnsureNotFree e = do
  v1 <- hnf e
  case v1 of
    Var _ -> error "Eval.ensureNotFree: suspended"
    _     -> return v1

--- Concolic evaluation of `failed`
ceBuiltinFailed :: [Expr] -> CEM Expr
ceBuiltinFailed es = case es of
  [] -> failS
  _  -> error "Eval.ceBuiltinFailed"

--- Concolic evaluation of `error`
ceBuiltinError :: Expr -> CEM Expr
ceBuiltinError e = do
  v1 <- nf e
  case v1 of
    Comb ConsCall _ _ -> error $ fromFCY v1
    _                 -> ceBuiltinError v1

--- Concolic evaluation of `success`
ceBuiltinSuccess :: [Expr] -> CEM Expr
ceBuiltinSuccess es = case es of
  [] -> succeedS
  _  -> error "Eval.ceBuiltinSuccess"

--- Concolic evaluation of `unknown`
ceBuiltinUnknown :: [Expr] -> CEM Expr
ceBuiltinUnknown es = case es of
  [] -> unknownS
  _  -> error "Eval.ceBuiltinUnknown"

--- Concolic evaluation of `(?)`
ceBuiltinChoice :: Expr -> Expr -> CEM Expr
ceBuiltinChoice = hnfOr

--- Concolic evaluation of a concurrent conjunction `(&)`
ceBuiltinAmp :: Expr -> Expr -> CEM Expr
ceBuiltinAmp e1 e2 = hnf e1 >>= \v1 -> case v1 of
  Var i -> lookupBinding i >>= \mbdg -> case mbdg of
    Just FreeVar       -> bindE i trueExpr >> hnf e2
    Just LazyFree      -> bindE i trueExpr >> hnf e2
    _                  -> other v1
  _ | v1 == trueExpr   -> hnf e2
    | v1 == failedExpr -> failS
    | otherwise        -> other v1
 where
  other v1 = hnf e2 >>= \v2 -> case v2 of
    _ | v2 == trueExpr   -> hnf v1
      | v2 == failedExpr -> failS
      | otherwise        -> ceBuiltinAmp v1 v2

--- Concolic evaluation of `negateFloat`
ceBuiltinNegFloat :: Expr -> CEM Expr
ceBuiltinNegFloat e = do
  v <- hnf e
  case v of
    Lit (Floatc l) -> return (toFCY (negate l))
    _              -> ceBuiltinNegFloat v

--- Concolic evaluation of `ord`
ceBuiltinOrd :: Expr -> CEM Expr
ceBuiltinOrd e = hnf e >>= \v -> case v of
  Lit (Charc l) -> return (toFCY (ord l))
  _             -> ceBuiltinOrd v

--- Concolic evaluation of `chr`
ceBuiltinChr :: Expr -> CEM Expr
ceBuiltinChr e = hnf e >>= \v -> case v of
  Lit (Intc l) -> return (toFCY (chr l))
  _            -> ceBuiltinChr v

--- Concolic evaluation of `i2f`
ceBuiltinI2F :: Expr -> CEM Expr
ceBuiltinI2F e = do
  v <- hnf e
  case v of
    Lit (Intc l) -> return (toFCY (fromInteger l))
    _            -> ceBuiltinI2F v

--- Concolic evaluation of `(&&)`
ceBuiltinAnd :: Expr -> Expr -> CEM Expr
ceBuiltinAnd e1 e2 = hnf $ Case Flex e1
  [ Branch (Pattern (prel "False") []) falseExpr
  , Branch (Pattern (prel "True" ) []) e2
  ]

--- Concolic evaluation of `(||)`
ceBuiltinOr :: Expr -> Expr -> CEM Expr
ceBuiltinOr e1 e2 = hnf $ Case Flex e1
  [ Branch (Pattern (prel "True" ) []) trueExpr
  , Branch (Pattern (prel "False") []) e2
  ]

--- Concolic evaluation of `($!)` / `($!!)`
ceBuiltinDollarBangs :: (Expr -> CEM Expr) -> Expr -> Expr -> CEM Expr
ceBuiltinDollarBangs f e1 e2 = f e2 >>= ceBuiltinApply e1

--- Concolic evaluation of `($##)`
ceBuiltinDollarHashHash :: Expr -> Expr -> CEM Expr
ceBuiltinDollarHashHash e1 e2 = do
  v2 <- nf e2
  case v2 of
    Var _ -> ceBuiltinDollarHashHash e1 v2
    _     -> ceBuiltinApply e1 v2

--- Concolic evaluation of `(=:=)`
ceBuiltinUni :: Expr -> Expr -> CEM Expr
ceBuiltinUni e1 e2 = do
  v1 <- hnf e1
  v2 <- hnf e2
  h  <- getHeap
  case (v1, v2) of
    (Var i, Var j)
      | i == j    -> lookupBinding i >>= \mbdg -> case mbdg of
        Just FreeVar  -> succeedS
        Just LazyFree -> succeedS
        _             -> ceBuiltinUni v1 v2
      | otherwise -> lookupBinding i >>= \mbdg -> case mbdg of
        Just FreeVar  -> uniFree
        Just LazyFree -> uniFree
        _             -> ceBuiltinUni v1 v2
      where uniFree = do
              bindE i v2
              mbdg <- lookupBinding j
              case mbdg of
                Just FreeVar  -> succeedS
                Just LazyFree -> succeedS
                _             -> ceBuiltinUni v1 v2
    (Var i, Lit _) -> lookupBinding i >>= \mbdg -> case mbdg of
      Just FreeVar  -> bindE i v2 >> succeedS
      Just LazyFree -> bindE i v2 >> succeedS
      _             -> ceBuiltinUni v1 v2
    (Lit _, Var j) -> lookupBinding j >>= \mbdg -> case mbdg of
      Just FreeVar  -> bindE j v1 >> succeedS
      Just LazyFree -> bindE j v1 >> succeedS
      _             -> ceBuiltinUni v2 v1
    (Lit l1, Lit l2)
      | l1 == l2   -> succeedS
      | otherwise  -> failS
    (Var i, e@(Comb ConsCall c es))
      | occurCheck i e h -> failS
      | otherwise        -> do
          js <- freshVars (length es)
          mapM_ bindF js
          let ys = map Var js
          bindE i (Comb ConsCall c ys)
          hnf $ combine (prel "&") (prel "=:=") trueExpr ys es
    (e@(Comb ConsCall c es), Var j)
      | occurCheck j e h -> failS
      | otherwise        -> do
          is <- freshVars (length es)
          mapM_ bindF is
          let ys = map Var is
          bindE j (Comb ConsCall c ys)
          hnf $ combine (prel "&") (prel "=:=") trueExpr es ys
    (Comb ConsCall c1 es1, Comb ConsCall c2 es2)
      | c1 == c2  -> hnf $ combine (prel "&") (prel "=:=") trueExpr es1 es2
      | otherwise -> failS
    _ | all (== trueExpr) [v1, v2] -> succeedS
      | otherwise                  -> ceBuiltinUni v1 v2

--- Concolic evaluation of `(=:<=)`
ceBuiltinLazyUni :: Expr -> Expr -> CEM Expr
ceBuiltinLazyUni e1 e2 = do
  v1 <- hnf e1
  case v1 of
    Var i -> lookupBinding i >>= \mbdg -> case mbdg of
      Just FreeVar  -> bindLE i e2 >> succeedS
      Just LazyFree -> bindF i >> hnf (Comb FuncCall (prel "=:=") [v1, e2])
      _             -> ceBuiltinLazyUni v1 e2
    _     -> do
      v2 <- hnf e2
      case (v1, v2) of
        (Lit l1, Lit l2)
          | l1 == l2   -> succeedS
          | otherwise  -> failS
        (Lit _, Var j) -> lookupBinding j >>= \mbdg -> case mbdg of
          Just FreeVar -> bindE j v1 >> succeedS
          _            -> ceBuiltinLazyUni v1 v2
        (Comb ConsCall c es, Var j) -> lookupBinding j >>= \mbdg -> case mbdg of
          Just FreeVar  -> lazyBindFree
          Just LazyFree -> lazyBindFree
          _             -> ceBuiltinLazyUni v1 v2
         where
          lazyBindFree = do
            is <- freshVars (length es)
            mapM_ bindF is
            let ys = map Var is
            bindE j (Comb ConsCall c ys)
            hnf $ combine (prel "&") (prel "=:<=") trueExpr es ys
        (Comb ConsCall c1 es1, Comb ConsCall c2 es2)
          | c1 == c2  -> hnf $ combine (prel "&") (prel "=:<=") trueExpr es1 es2
          | otherwise -> failS
        _             -> ceBuiltinUni v1 v2

--- Occur check for strict unification
occurCheck :: VarIndex -> Expr -> Heap -> Bool
occurCheck i e h = i `elem` freeVars e h
  where
  freeVars (Var         j) h' = case lookupH j h' of
    Just (BoundVar  e') -> freeVars e' (unbind j h')
    Just (LazyBound e') -> freeVars e' (unbind j h')
    _                  -> [j]
  freeVars (Lit         _) _ = []
  freeVars (Comb  ct _ es) h' = case ct of
    ConsCall -> nub $ concatMap (flip freeVars h') es
    _        -> []
  freeVars (Let     bs e') h' = freeVars e' h' \\ map fst bs
  freeVars (Free    vs e') h' = freeVars e' h' \\ vs
  freeVars (Or      e1 e2) h' = freeVars e1 h' `intersect` freeVars e2 h'
  freeVars (Case   _ _ bs) h' = foldr1 intersect (map freeBranch bs)
    where freeBranch (Branch p be) = freeVars be h' \\ patVars p
  freeVars (Typed    e' _) h' = freeVars e' h'

--- Concolic evaluation of binary operations on integers (arithmetic, logical)
-- Note that PAKCS applies most binary operators in reverse order.
-- For this reason, the arguments of `ceBuiltinIntOp` are switched internally.
ceBuiltinIntOp :: ToFCY a => (Int -> Int -> a) -> Expr -> Expr -> CEM Expr
ceBuiltinIntOp op e1 e2 = do
  v1 <- hnf e1
  v2 <- hnf e2
  case (v1, v2) of
    (Lit (Intc l1), Lit (Intc l2)) -> return $ toFCY (op l2 l1)
    _                              -> ceBuiltinIntOp op v1 v2

--- Concolic evaluation of binary operations on characters (arithmetic, logical)
ceBuiltinCharOp :: ToFCY a => (Char -> Char -> a) -> Expr -> Expr -> CEM Expr
ceBuiltinCharOp op e1 e2 = do
  v1 <- hnf e1
  v2 <- hnf e2
  case (v1, v2) of
    (Lit (Charc l1), Lit (Charc l2)) -> return $ toFCY (op l1 l2)
    _                                -> ceBuiltinCharOp op v1 v2

--- Concolic evaluation of binary operations on floats (arithmetic, logical)
-- Note that PAKCS applies most binary operators in reverse order.
-- For this reason, the arguments of `ceBuiltinFloatOp` are switched internally.
ceBuiltinFloatOp :: ToFCY a => (Float -> Float -> a) -> Expr -> Expr -> CEM Expr
ceBuiltinFloatOp op e1 e2 = do
  v1 <- hnf e1
  v2 <- hnf e2
  case (v1, v2) of
    (Lit (Floatc l1), Lit (Floatc l2)) -> return $ toFCY (op l2 l1)
    _                                  -> ceBuiltinFloatOp op v1 v2


--- FlatCurry representation of a call to a builtin function
builtin :: String -> [Expr] -> Expr
builtin f es = Comb FuncCall (prel f) es
