--- ----------------------------------------------------------------------------
--- This module provides operations to evaluate `FlatCurry` programs to
--- (head) normal form based on the natural semantics.
---
--- @author  Jan Tikovsky
--- @version December 2017
--- ----------------------------------------------------------------------------
module Eval where

-- TODO: remove
import FiniteMap (filterFM, maxFM)

import FlatCurry.Annotated.Goodies hiding (range)
import FlatCurry.Annotated.Pretty         (ppExp)
import FlatCurry.Annotated.Types

import List ((\\), find, intersect, nub)

import Text.Pretty hiding (combine)

import CCTOptions          (CCTOpts (..))
import FCYFunctorInstances
import FlatCurryGoodies
import Heap
import Output              (traceEval, traceInfo, traceStatus)
import Substitution
import Symbolic
import Utils

import Debug

infixl 3 <|>

-- -----------------------------------------------------------------------------
-- Concolic evaluation monad transformer
-- -----------------------------------------------------------------------------

--- Monad for non-deterministic results
--- @cons Return - single result
--- @cons Choice - non-deterministic choice
data Result a
  = Return a
  | Choice (Result a) (Result a)

instance Monad Result where
  return = Return

  m >>= f = case m of Return x   -> f x
                      Choice l r -> Choice (l >>= f) (r >>= f)

--- Concolic evaluation monad
data CEM a = CE { runCEM :: CEState -> Result (a, CEState) }

instance Monad CEM where
  return x = CE $ \s -> return (x, s)

  f >>= g = CE $ \s -> runCEM f s >>= \(x, s') -> runCEM (g x) s'

--- Internal state for concolic evaluation, consisting of
---   * the current ccti options,
---   * the concrete heap containing variable bindings,
---   * a list of all defined function declarations,
---   * an index for fresh variables,
---   * a flag which indicates whether a boolean case expression is evaluated:
---     This is required for the collection of constraints
---   * a trace of symbolic information for case branches and
---   * a counter of the remaining number of evaluation steps
data CEState = CEState
  { cesCCTOpts   :: CCTOpts
  , cesHeap      :: Heap
  , cesFuncs     :: [AFuncDecl TypeAnn]
  , cesFresh     :: VarIndex
  , cesTraceFlag :: Bool
  , cesTrace     :: Trace
  , cesSteps     :: Int
  , cesSymHeap   :: Heap
  , cesLConstr   :: Maybe (AExpr TypeAnn)
  }

--- Initial state for concolic evaluation
initCEState :: CCTOpts -> AExpSubst -> [AFuncDecl TypeAnn] -> VarIndex -> CEState
initCEState opts sub fs v = CEState
  { cesCCTOpts   = opts
  , cesHeap      = fromSubst sub
  , cesFuncs     = fs
  , cesFresh     = v
  , cesTraceFlag = False
  , cesTrace     = []
  , cesSteps     = 0
  , cesSymHeap   = foldr bindSym emptyH (dom sub)
  , cesLConstr   = Nothing
  }

--- Combine two evaluations within a choice
choice :: CEM a -> CEM a -> CEM a
choice x y = CE $ \s -> Choice (runCEM x s) (runCEM y s)

--- Infix version of non-deterministic choice.
(<|>) :: CEM a -> CEM a -> CEM a
(<|>) = choice

gets :: (CEState -> a) -> CEM a
gets f = CE $ \s -> Return (f s, s)

get :: CEM CEState
get = gets id

put :: CEState -> CEM ()
put s = CE $ \_ -> Return ((), s)

modify :: (CEState -> CEState) -> CEM ()
modify f = CE $ \s -> Return ((), f s)

--- Generate a Heap from a given Substitution
fromSubst :: AExpSubst -> Heap
fromSubst sub = fromListH $ zip (dom sub) $ map BoundVar (range sub)

--- Trace a single evaluation step
traceStep :: String -> AExpr TypeAnn -> CEM (AExpr TypeAnn) -> CEM (AExpr TypeAnn)
traceStep ruleName e x = do
  opts <- getOpts
  h    <- getHeap
  t    <- getTrace
  s    <- countStep
  traceEval opts
    (pPrint $ vsep [ text "Evaluation step:" <+> int s
                   , text "Rule Name:"       <+> text ruleName
                   , text "Heap:"            <+> ppHeap h
                   , text "Symbolic Trace:"  <+> listSpaced (map pretty t)
                   , text "Expression:"      <+> ppExp e
                   ])
    (if optEvalSteps opts - s < 0 then traceStatus opts msg (return e) else x)
 where msg = "Maximum number of evaluation steps exceeded. Aborting evaluation."

traceSym :: CEState -> a -> a
traceSym s = traceInfo (cesCCTOpts s)
  (pPrint $ text "Symbolic Trace:" <+> listSpaced (map pretty (reverse (cesTrace s))))

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
bindE :: VarIndex -> AExpr TypeAnn -> CEM ()
bindE v e = modifyHeap (bindExpr v e)

--- Bind a variable lazily to an expression
bindLE :: VarIndex -> AExpr TypeAnn -> CEM ()
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
bindArg :: AExpr TypeAnn -> CEM (AExpr TypeAnn)
bindArg e = case e of
  AVar      _ _ -> return e
  ALet  _ bs e' -> addBindings bs e' >>= bindArg
  AFree _ vs e' -> addFrees    vs e' >>= bindArg
  _              -> do v <- freshVar
                       bindE v e
                       return (AVar (annExpr e) v)

--- ----------------------------------------------------------------------------

--- Lookup the parameters and the rhs for a given function
lookupRule :: QName -> CEM (Maybe ([VarIndex], AExpr TypeAnn))
lookupRule f = do
  mf <- gets (find (hasName f) . cesFuncs)
  return $ case mf of
    Just (AFunc _ _ _ _ (ARule _ vs rhs)) -> Just (map fst vs, rhs)
    _                                     -> Nothing

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

getTraceFlag :: CEM Bool
getTraceFlag = gets cesTraceFlag

--- Set the trace flag to the given boolean
setTraceFlag :: Bool -> CEM ()
setTraceFlag b = modify $ \s -> s { cesTraceFlag = b }

--- Perform the given action with tracing of constraints on literals
withLitTracing :: CEM a -> CEM a
withLitTracing act = do
  setTraceFlag True
  res <- act
  setTraceFlag False
  return res

--- ----------------------------------------------------------------------------

--- Get the trace of symbolic information
getTrace :: CEM Trace
getTrace = gets cesTrace

--- Add a decision with symbolic information for case expression `cid` to the trace
mkDecision :: AExpr TypeAnn -> CaseID -> BranchNr -> VarIndex
           -> (QName, TypeAnn) -> [VarIndex] -> CEM ()
mkDecision e cid bnr v c args
  | isOtherwise e || isDict qn = return ()
  | otherwise = do
      mce <- getResetLConstr >>= getSymConstr qn
      let (v', sobj) = maybe (v, SymCons qn ty) id mce
      modify $ \s -> s { cesTrace = (Decision cid bnr v' sobj args) : cesTrace s }
 where
  (qn, ty) = mapSnd fst3 c

--- ----------------------------------------------------------------------------

countStep :: CEM Int
countStep = do
  s <- get
  let n = cesSteps s + 1
  put s { cesSteps = n }
  return n

--- ----------------------------------------------------------------------------

-- TODO: Use symbolic heap instead of Maybe (LConstr, [AExpr TypeExpr])
-- Overthink/refactor/remove this part of the implementation

setLConstr :: Maybe (AExpr TypeAnn) -> CEM ()
setLConstr mce = modify $ \s -> s { cesLConstr = mce }

getResetLConstr :: CEM (Maybe (AExpr TypeAnn))
getResetLConstr = do
  mce <- gets cesLConstr
  setLConstr Nothing
  return mce

traceConstr :: (AExpr TypeAnn) -> CEM ()
traceConstr ce = getTraceFlag >>= \b -> whenM b (setLConstr (Just ce))

getSymConstr :: QName -> Maybe (AExpr TypeAnn) -> CEM (Maybe (VarIndex, SymObj))
getSymConstr _  Nothing  = return Nothing
getSymConstr qn (Just e) = case e of
  AComb _ FuncCall (fqn, _) [AVar _ i, AVar _ j] -> do
    let mlc = fmap (if snd qn == "False" then lcNeg else id) (lookup fqn litConstrs)
    mkSymConstr mlc i j
  _                                             -> return Nothing
 where
  mkSymConstr mlc i j = do
    mbi <- getOrigVI i
    mbj <- getOrigVI j
    return $ case (mlc, mbi, mbj) of
      (Just lc, Just (i', BoundVar (ALit a1 l1)), Just (j', BoundVar (ALit a2 l2)))
        | trd3 a1 && not (trd3 a2) -> Just (j', SymLit lc            l1)
        | trd3 a2 && not (trd3 a1) -> Just (i', SymLit (lcMirror lc) l2) -- due to application of binary operators in reverse order
      _                            -> Nothing

-- very ugly => replace by symbolic heap implementation
getOrigVI :: VarIndex -> CEM (Maybe (VarIndex, Binding))
getOrigVI i = do
  mbi <- lookupBinding i
  h   <- getHeap
  case mbi of
    Nothing -> return Nothing
    Just b  -> return (maxFM (filterFM (\_ b' -> b == b') h))

--- ----------------------------------------------------------------------------

--- concolic evaluation
ceval :: AExpr TypeAnn -> CEState -> ([AExpr TypeAnn], [Trace], VarIndex)
ceval e s = fromResult $ runCEM (nf e) s

--- Prepare expression for narrowing
prepExpr :: AExpr TypeAnn -> VarIndex -> (AExpr TypeAnn, [VarIndex])
prepExpr e v = case e of
  AComb ty FuncCall f es ->
    let n  = length es - 1
        vs = [v .. v + n]
    in (AFree (annExpr e)
              (zip vs (map annExpr es))
              (AComb ty FuncCall f (zipWith toVar es vs)), vs)
  _                      -> (e, [])
 where
 toVar e' vi = if isFuncPartCall e' then e' else AVar (annExpr e') vi

--- Normalize FlatCurry function call
norm :: VarIndex -> AExpr TypeAnn -> (AExpSubst, AExpr TypeAnn, VarIndex)
norm v e = case e of
  AComb ty FuncCall f es ->
    let vs = [v, v-1 ..]
        s  = mkSubst vs es
    in (s, AComb ty FuncCall f (zipWith AVar (map annExpr es) vs), v - length es)
  _                      -> (emptySubst, e, v)

--- Select the result, the symbolic trace and the next free index
--- from the search tree of non-deterministic results
fromResult :: Result (AExpr TypeAnn, CEState)
           -> ([AExpr TypeAnn], [Trace], VarIndex)
fromResult (Return (e,s)) = traceSym s ([e], [reverse (cesTrace s)], cesFresh s)
fromResult (Choice e1 e2) = let (r1, t1, v1) = fromResult e1
                                (r2, t2, v2) = fromResult e2
                            in (r1 ++ r2, t1 ++ t2, min v1 v2)

--- Evaluate given FlatCurry expression to normal form
nf :: AExpr TypeAnn -> CEM (AExpr TypeAnn)
nf e = hnf e >>= \e' -> case e' of
  AComb ty ConsCall c es -> AComb ty ConsCall c <$> mapM nf es
  _                      -> return e'

--- Evaluate given FlatCurry expression to head normal form
hnf :: AExpr TypeAnn -> CEM (AExpr TypeAnn)
hnf exp = case exp of
  AVar        ty v  -> traceStep "Var"  exp $ hnfVar  ty v
  ALit        ty l  -> traceStep "Lit"  exp $ hnfLit  ty l
  AComb ty ct f es  -> traceStep "Comb" exp $ hnfComb ty ct f es
  ALet      _ bs e  -> traceStep "Let"  exp $ hnfLet  bs e
  AFree     _ vs e  -> traceStep "Free" exp $ hnfFree vs e
  AOr      _ e1 e2  -> traceStep "Or"   exp $ hnfOr   e1 e2
  ACase ann ct e bs -> traceStep "Case" exp $ hnfCase ann ct e bs
  ATyped      _ e _ -> hnf e

--- Concolic evaluation of a variable
hnfVar :: TypeAnn -> VarIndex -> CEM (AExpr TypeAnn)
hnfVar ty i = lookupBinding i >>= \mbdg -> case mbdg of
  Nothing            -> failS ty
  Just BlackHole     -> failS ty
  Just (BoundVar  e) -> bindBH i >> hnf e >>= \v -> bindE i v >> return v
  Just (LazyBound e) -> bindBH i >> hnf e >>= bindAndCheckLazy
  Just FreeVar       -> return (AVar ty i)
  Just LazyFree      -> return (AVar ty i)
 where
  bindAndCheckLazy v = case v of
    -- variable
    AVar ty' w -> bindLE i v >> lookupBinding w >>= \mbdg -> case mbdg of
      Nothing                -> failS ty'
      Just BlackHole         -> failS ty'
      Just (BoundVar      e) -> bindLE w e >> return v
      Just FreeVar           -> bindLF w   >> return v
      _                      -> return v
    -- literal
    ALit                 _ _ -> bindE i v >> return v
    AComb ty' ConsCall qn xs -> do
      ys <- freshVars (length xs)
      let val = AComb ty' ConsCall qn (zipWith AVar (map annExpr xs) ys)
      zipWithM_ bindLE ys xs
      bindE i val
      return val
    _                        -> error $ "Eval.bindAndCheckLazy: " ++ show v

--- Concolic evaluation of a literal
hnfLit :: TypeAnn -> Literal -> CEM (AExpr TypeAnn)
hnfLit ty l = return (ALit ty l)

--- Concolic evaluation of a combination:
---   * flattened function calls: check if function is builtin or user-defined
---                               and start corresponding concolic evaluation
---   * non-flattened function/constructor calls: flatten them
hnfComb :: TypeAnn -> CombType -> (QName, TypeAnn) -> [AExpr TypeAnn]
        -> CEM (AExpr TypeAnn)
hnfComb ty ct f@(qn, _) es = case ct of
  FuncCall
    | all isVar es -> lookupRule qn >>= \mrule -> case mrule of
      Nothing      -> ceBuiltin ty f es
      Just (xs, e) -> hnf (subst (mkSubst xs es) e)
    | otherwise    -> mvs >>= \vs -> hnf (AComb ty ct f vs)
  _                -> AComb ty ct f <$> mvs
 where
  mvs = mapM bindArg es

--- Concolic evaluation of a let expression
hnfLet :: [((VarIndex, TypeAnn), AExpr TypeAnn)] -> AExpr TypeAnn
       -> CEM (AExpr TypeAnn)
hnfLet bs e = addBindings bs e >>= hnf

--- Add a list of local bindings to the heap
addBindings :: [((VarIndex, TypeAnn), AExpr TypeAnn)] -> AExpr TypeAnn
            -> CEM (AExpr TypeAnn)
addBindings bs e = do
  ys <- freshVars (length bs)
  let (txs, es) = unzip bs
      (xs, tys) = unzip txs
      sigma     = mkSubst xs (zipWith AVar tys ys)
  zipWithM_ bindE ys (map (subst sigma) es)
  return (subst sigma e)

--- Concolic evaluation of a declaration of free variables
hnfFree :: [(VarIndex, TypeAnn)] -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
hnfFree vs e = addFrees vs e >>= hnf

--- Add a list of free variables to the heap
addFrees :: [(VarIndex, TypeAnn)] -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
addFrees tvs e = do
  ys <- freshVars (length tvs)
  mapM_ bindF ys
  let (vs, tys) = unzip tvs
  return $ subst (mkSubst vs (zipWith AVar tys ys)) e

--- Concolic evaluation of a non-deterministic choice
hnfOr :: AExpr TypeAnn -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
hnfOr e1 e2 = hnf e1 <|> hnf e2

--- Concolic evaluation of a case expression
hnfCase :: TypeAnn -> CaseType -> AExpr TypeAnn -> [ABranchExpr TypeAnn]
        -> CEM (AExpr TypeAnn)
hnfCase ann ct e bs = do
  ve@(AVar _ vi) <- bindArg e -- flattening of scrutinized expression
  -- Evaluate case argument with or without tracing of constraints on literals
  let hnfCaseArg = if hasBoolType e then (withLitTracing . hnf) else hnf
  hnfCaseArg ve >>= \v -> case v of
    ALit ty l -> case findBranch (ALPattern ty l) bs of
      Nothing          -> failS ty
      Just (_, _,  be) -> hnf be
    AComb ty ConsCall c es -> case findBranch (APattern ty c []) bs of
      Nothing          -> failS ty
      Just (n, vs, be)
        | all isVar es -> do
          mkDecision e cid (BNr n bcnt) vi c (map varNr es)
          hnf (subst (mkSubst vs es) be)
        | otherwise    -> hnf e
    AComb _ FuncCall _ _
      | v == failedExpr ty -> failS ty
      where ty = annExpr v
    AVar _ i
      | ct == Rigid -> error "Suspended"
      | otherwise   -> narrowCase
      where
        narrowCase = case bs of
          [] -> error "Eval.narrowCase: Found case expression without branches"
          _  -> foldr1 choice $ zipWith guess [1 ..] bs

        guess _ (ABranch (ALPattern  ty' l) be) = do
          bindE i (ALit ty' l)
          hnf be
        guess n (ABranch (APattern ty' c txs) be) = do
          ys  <- freshVars (length txs)
          mapM_ bindF ys
          mkDecision e cid (BNr n bcnt) vi c ys
          let (xs, tys) = unzip txs
              es'       = zipWith AVar tys ys
          bindE i (AComb ty' ConsCall c es')
          hnf (subst (mkSubst xs es') be)
    _ -> hnf v
 where
  cid  = cidAnn ann
  bcnt = length bs

--- Failing evaluation
failS :: TypeAnn -> CEM (AExpr TypeAnn)
failS ann = return (failedExpr ann)

--- Evaluation to `True`
succeedS :: CEM (AExpr TypeAnn)
succeedS = return trueExpr

--- Evaluation which yields a free variable
unknownS :: TypeAnn -> CEM (AExpr TypeAnn)
unknownS ann = do
  v <- freshVar
  bindF v
  return (AVar ann v)

-- -----------------------------------------------------------------------------
-- Concolic evaluation of builtin functions
-- -----------------------------------------------------------------------------

ceBuiltin :: TypeAnn -> (QName, TypeAnn) -> [AExpr TypeAnn]
          -> CEM (AExpr TypeAnn)
ceBuiltin ty f@(qn, _) es = case snd qn of
  "apply"            -> binary ceBuiltinApply             es
  "cond"             -> binary ceBuiltinCond              es
  "ensureNotFree"    -> unary  ceBuiltinEnsureNotFree     es
  "failed"           -> ceBuiltinFailed                ty es
  "prim_error"       -> unary  ceBuiltinError             es
  "success"          -> ceBuiltinSuccess                  es
  "unknown"          -> ceBuiltinUnknown               ty es
  "?"                -> binary ceBuiltinChoice            es
  "&"                -> binary ceBuiltinAmp               es
  "&>"               -> binary ceBuiltinCond              es
--   "&&"               -> binary ceBuiltinAnd               es
--   "||"               -> binary ceBuiltinOr                es
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
  "prim_eqInt"       -> traceConstr ce >> binary (ceBuiltinIntOp (==)) es
  "prim_ltEqInt"     -> traceConstr ce >> binary (ceBuiltinIntOp (<=)) es
--   "_==" -> binary (ceBuiltinIntOp (==)) es
--   "_/=" -> binary (ceBuiltinIntOp (/=)) es
--   "_<"  -> binary (ceBuiltinIntOp (<)) es
--   "_<=" -> binary (ceBuiltinIntOp (<=)) es
--   "_>"  -> binary (ceBuiltinIntOp (>)) es
--   "_>=" -> binary (ceBuiltinIntOp (>=)) es

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
 where
  ce = AComb ty FuncCall f es

unary :: (AExpr TypeAnn -> CEM (AExpr TypeAnn)) -> [AExpr TypeAnn]
      -> CEM (AExpr TypeAnn)
unary f es = case es of
  [e] -> f e
  _   -> error "Eval unary"

binary :: (AExpr TypeAnn -> AExpr TypeAnn -> CEM (AExpr TypeAnn))
       -> [AExpr TypeAnn] -> CEM (AExpr TypeAnn)
binary f es = case es of
  [e1,e2] -> f e1 e2
  _       -> error "Eval.binary"

--- Concolic evaluation of a higher order application
ceBuiltinApply :: AExpr TypeAnn -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinApply e1 e2 = hnf e1 >>= \v1 -> case v1 of
  AComb ty ct f es | isPartCall ct -> hnf $ addPartCallArg ty ct f es e2
  _                                -> ceBuiltinApply v1 e2

--- Concolic evaluation of a conditional expression `(&>)` / `cond`
ceBuiltinCond :: AExpr TypeAnn -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinCond e1 e2 = hnf e1 >>= \v1 -> case v1 of
  AComb _ FuncCall g [x ,y]
      -- replace `(e1 &> e2) &> e3` by `e1 &> (e2 &> e3)`
    | fst g == prel "&>"   -> hnf $ builtin cty1 "&>"   [x, builtin cty2 "&>" [y, e2]]
    | fst g == prel "cond" -> hnf $ builtin cty1 "cond" [x, builtin cty2 "cond" [y, e2]]
  _ | v1 == trueExpr       -> hnf e2
    | v1 == failedExpr ann -> failS ann
    | otherwise            -> ceBuiltinCond v1 e2
 where
  cty1 = condAnn boolType
  cty2 = condAnn (tyAnn ann)
  ann  = annExpr e2

--- Concolic evaluation of `ensureNotFree`
ceBuiltinEnsureNotFree :: AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinEnsureNotFree e = do
  v1 <- hnf e
  case v1 of
    AVar _ _ -> error "Eval.ensureNotFree: suspended"
    _        -> return v1

--- Concolic evaluation of `failed`
ceBuiltinFailed :: TypeAnn -> [AExpr TypeAnn] -> CEM (AExpr TypeAnn)
ceBuiltinFailed ty es = case es of
  [] -> failS ty
  _  -> error "Eval.ceBuiltinFailed"

--- Concolic evaluation of `error`
ceBuiltinError :: AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinError e = do
  v1 <- nf e
  case v1 of
    AComb _ ConsCall _ _ -> error $ fromFCY v1
    _                    -> ceBuiltinError v1

--- Concolic evaluation of `success`
ceBuiltinSuccess :: [AExpr TypeAnn] -> CEM (AExpr TypeAnn)
ceBuiltinSuccess es = case es of
  [] -> succeedS
  _  -> error "Eval.ceBuiltinSuccess"

--- Concolic evaluation of `unknown`
ceBuiltinUnknown :: TypeAnn -> [AExpr TypeAnn] -> CEM (AExpr TypeAnn)
ceBuiltinUnknown ty es = case es of
  [] -> unknownS ty
  _  -> error "Eval.ceBuiltinUnknown"

--- Concolic evaluation of `(?)`
ceBuiltinChoice :: AExpr TypeAnn -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinChoice = hnfOr

--- Concolic evaluation of a concurrent conjunction `(&)`
ceBuiltinAmp :: AExpr TypeAnn -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinAmp e1 e2 = hnf e1 >>= \v1 -> case v1 of
  AVar _ i -> lookupBinding i >>= \mbdg -> case mbdg of
    Just FreeVar               -> bindE i trueExpr >> hnf e2
    Just LazyFree              -> bindE i trueExpr >> hnf e2
    _                          -> other v1
  _ | v1 == trueExpr           -> hnf e2
    | v1 == failedExpr boolAnn -> failS boolAnn
    | otherwise                -> other v1
 where
  other v1 = hnf e2 >>= \v2 -> case v2 of
    _ | v2 == trueExpr           -> hnf v1
      | v2 == failedExpr boolAnn -> failS boolAnn
      | otherwise                -> ceBuiltinAmp v1 v2

--- Concolic evaluation of `negateFloat`
ceBuiltinNegFloat :: AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinNegFloat e = do
  v <- hnf e
  case v of
    ALit _ (Floatc l) -> return (toFCY (negate l))
    _                 -> ceBuiltinNegFloat v

--- Concolic evaluation of `ord`
ceBuiltinOrd :: AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinOrd e = hnf e >>= \v -> case v of
  ALit _ (Charc l) -> return (toFCY (ord l))
  _                -> ceBuiltinOrd v

--- Concolic evaluation of `chr`
ceBuiltinChr :: AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinChr e = hnf e >>= \v -> case v of
  ALit _ (Intc l) -> return (toFCY (chr l))
  _               -> ceBuiltinChr v

--- Concolic evaluation of `i2f`
ceBuiltinI2F :: AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinI2F e = do
  v <- hnf e
  case v of
    ALit _ (Intc l) -> return (toFCY (fromInteger l))
    _               -> ceBuiltinI2F v

--- Concolic evaluation of `(&&)`
-- ceBuiltinAnd :: AExpr TypeAnn -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
-- ceBuiltinAnd e1 e2 = hnf $ ACase boolType Flex e1
--   [ ABranch (APattern boolType (prel "False", boolType) []) falseExpr
--   , ABranch (APattern boolType (prel "True",  boolType) []) e2
--   ]

--- Concolic evaluation of `(||)`
-- ceBuiltinOr :: AExpr TypeAnn -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
-- ceBuiltinOr e1 e2 = hnf $ ACase boolType Flex e1
--   [ ABranch (APattern boolType (prel "True",  boolType) []) trueExpr
--   , ABranch (APattern boolType (prel "False", boolType) []) e2
--   ]

--- Concolic evaluation of `($!)` / `($!!)`
ceBuiltinDollarBangs :: (AExpr TypeAnn -> CEM (AExpr TypeAnn))
                     -> AExpr TypeAnn -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinDollarBangs f e1 e2 = f e2 >>= ceBuiltinApply e1

--- Concolic evaluation of `($##)`
ceBuiltinDollarHashHash :: AExpr TypeAnn -> AExpr TypeAnn
                        -> CEM (AExpr TypeAnn)
ceBuiltinDollarHashHash e1 e2 = do
  v2 <- nf e2
  case v2 of
    AVar _ _ -> ceBuiltinDollarHashHash e1 v2
    _        -> ceBuiltinApply e1 v2

--- Concolic evaluation of `(=:=)`
ceBuiltinUni :: AExpr TypeAnn -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinUni e1 e2 = do
  v1 <- hnf e1
  v2 <- hnf e2
  h  <- getHeap
  case (v1, v2) of
    (AVar _ i, AVar _ j)
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
    (AVar _ i, ALit _ _) -> lookupBinding i >>= \mbdg -> case mbdg of
      Just FreeVar  -> bindE i v2 >> succeedS
      Just LazyFree -> bindE i v2 >> succeedS
      _             -> ceBuiltinUni v1 v2
    (ALit _ _, AVar _ j) -> lookupBinding j >>= \mbdg -> case mbdg of
      Just FreeVar  -> bindE j v1 >> succeedS
      Just LazyFree -> bindE j v1 >> succeedS
      _             -> ceBuiltinUni v2 v1
    (ALit _ l1, ALit _ l2)
      | l1 == l2   -> succeedS
      | otherwise  -> failS boolAnn
    (AVar _ i, e@(AComb ty ConsCall c es))
      | occurCheck i e h -> failS boolAnn
      | otherwise        -> do
          js <- freshVars (length es)
          mapM_ bindF js
          let ys = zipWith AVar (map annExpr es) js
          bindE i (AComb ty ConsCall c ys)
          hnf $ combine (prel "&") (prel "=:=") trueExpr ys es
    (e@(AComb ty ConsCall c es), AVar _ j)
      | occurCheck j e h -> failS boolAnn
      | otherwise        -> do
          is <- freshVars (length es)
          mapM_ bindF is
          let ys = zipWith AVar (map annExpr es) is
          bindE j (AComb ty ConsCall c ys)
          hnf $ combine (prel "&") (prel "=:=") trueExpr es ys
    (AComb _ ConsCall c1 es1, AComb _ ConsCall c2 es2)
      | c1 == c2  -> hnf $ combine (prel "&") (prel "=:=") trueExpr es1 es2
      | otherwise -> failS boolAnn
    _ | all (== trueExpr) [v1, v2] -> succeedS
      | otherwise                  -> ceBuiltinUni v1 v2

--- Concolic evaluation of `(=:<=)`
ceBuiltinLazyUni :: AExpr TypeAnn -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinLazyUni e1 e2 = do
  v1 <- hnf e1
  case v1 of
    AVar ty i -> lookupBinding i >>= \mbdg -> case mbdg of
      Just FreeVar  -> bindLE i e2 >> succeedS
      Just LazyFree -> bindF i >> hnf (AComb boolAnn FuncCall (prel "=:=", unifyAnn ty) [v1, e2])
      _             -> ceBuiltinLazyUni v1 e2
    _     -> do
      v2 <- hnf e2
      case (v1, v2) of
        (ALit _ l1, ALit _ l2)
          | l1 == l2   -> succeedS
          | otherwise  -> failS boolAnn
        (ALit _ _, AVar _ j) -> lookupBinding j >>= \mbdg -> case mbdg of
          Just FreeVar -> bindE j v1 >> succeedS
          _            -> ceBuiltinLazyUni v1 v2
        (AComb ty ConsCall c es, AVar _ j) -> lookupBinding j >>= \mbdg -> case mbdg of
          Just FreeVar  -> lazyBindFree
          Just LazyFree -> lazyBindFree
          _             -> ceBuiltinLazyUni v1 v2
         where
          lazyBindFree = do
            is <- freshVars (length es)
            mapM_ bindF is
            let ys = zipWith AVar (map annExpr es) is
            bindE j (AComb ty ConsCall c ys)
            hnf $ combine (prel "&") (prel "=:<=") trueExpr es ys
        (AComb _ ConsCall c1 es1, AComb _ ConsCall c2 es2)
          | c1 == c2  -> hnf $ combine (prel "&") (prel "=:<=") trueExpr es1 es2
          | otherwise -> failS boolAnn
        _             -> ceBuiltinUni v1 v2

--- Occur check for strict unification
occurCheck :: VarIndex -> AExpr TypeAnn -> Heap -> Bool
occurCheck i e h = i `elem` freeVars e h
  where
  freeVars (AVar        _ j) h' = case lookupH j h' of
    Just (BoundVar  e') -> freeVars e' (unbind j h')
    Just (LazyBound e') -> freeVars e' (unbind j h')
    _                  -> [j]
  freeVars (ALit        _ _) _  = []
  freeVars (AComb _ ct _ es) h' = case ct of
    ConsCall -> nub $ concatMap (flip freeVars h') es
    _        -> []
  freeVars (ALet    _ bs e') h' = freeVars e' h' \\ map fst (map fst bs)
  freeVars (AFree   _ vs e') h' = freeVars e' h' \\ map fst vs
  freeVars (AOr     _ e1 e2) h' = freeVars e1 h' `intersect` freeVars e2 h'
  freeVars (ACase  _ _ _ bs) h' = foldr1 intersect (map freeBranch bs)
    where freeBranch (ABranch p be) = freeVars be h' \\ patVars p
  freeVars (ATyped   _ e' _) h' = freeVars e' h'

--- Concolic evaluation of binary operations on integers (arithmetic, logical)
-- Note that PAKCS applies most binary operators in reverse order.
-- For this reason, the arguments of `ceBuiltinIntOp` are switched internally.
ceBuiltinIntOp :: ToFCY a => (Int -> Int -> a) -> AExpr TypeAnn
               -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinIntOp op e1 e2 = do
  v1 <- hnf e1
  v2 <- hnf e2
  case (v1, v2) of
    (ALit _ (Intc l1), ALit _ (Intc l2)) -> return $ toFCY (op l2 l1)
    _                                    -> ceBuiltinIntOp op v1 v2

--- Concolic evaluation of binary operations on characters (arithmetic, logical)
ceBuiltinCharOp :: ToFCY a => (Char -> Char -> a) -> AExpr TypeAnn
                -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinCharOp op e1 e2 = do
  v1 <- hnf e1
  v2 <- hnf e2
  case (v1, v2) of
    (ALit _ (Charc l1), ALit _ (Charc l2)) -> return $ toFCY (op l1 l2)
    _                                      -> ceBuiltinCharOp op v1 v2

--- Concolic evaluation of binary operations on floats (arithmetic, logical)
-- Note that PAKCS applies most binary operators in reverse order.
-- For this reason, the arguments of `ceBuiltinFloatOp` are switched internally.
ceBuiltinFloatOp :: ToFCY a => (Float -> Float -> a) -> AExpr TypeAnn
                 -> AExpr TypeAnn -> CEM (AExpr TypeAnn)
ceBuiltinFloatOp op e1 e2 = do
  v1 <- hnf e1
  v2 <- hnf e2
  case (v1, v2) of
    (ALit _ (Floatc l1), ALit _ (Floatc l2)) -> return $ toFCY (op l2 l1)
    _                                        -> ceBuiltinFloatOp op v1 v2

--- FlatCurry representation of a call to a builtin function
builtin :: TypeAnn -> String -> [AExpr TypeAnn] -> AExpr TypeAnn
builtin ann f es = AComb (trTpl3 resultType id id ann) FuncCall (prel f, ann) es
