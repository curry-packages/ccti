--- ----------------------------------------------------------------------------
--- This module provides operations to evaluate `FlatCurry` programs to
--- (head) normal form based on the natural semantics.
---
--- @author  Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module Eval where

import FlatCurry.Annotated.Goodies
import FlatCurry.Annotated.Types
import List                        ((\\), find, intersect, nub)

import CCTOptions                  (CCTOpts (..))
import FlatCurryGoodies
import Heap
import Output                      (traceDebug, traceInfo)
import PrettyPrint hiding          (combine)
import Substitution
import Symbolic
import Utils

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
---   * the argument variables of the function which is concolically tested
---   * a trace of symbolic information for case branches.
data CEState = CEState
  { cesCCTOpts :: CCTOpts
  , cesHeap    :: Heap
  , cesFuncs   :: [AFuncDecl TypeExpr]
  , cesFresh   :: VarIndex
  , cesArgs    :: [VarIndex]
  , cesTrace   :: Trace
  }

--- Initial state for concolic evaluation
initState :: CCTOpts -> [AFuncDecl TypeExpr] -> VarIndex -> CEState
initState opts fs v = CEState
  { cesCCTOpts = opts
  , cesHeap    = emptyH
  , cesFuncs   = fs
  , cesFresh   = v
  , cesArgs    = []
  , cesTrace   = []
  }

--- Trace a single evaluation step
traceStep :: AExpr TypeExpr -> CEM a -> CEM a
traceStep e x = do
  opts <- getOpts
  h    <- getHeap
  t    <- getTrace
  traceDebug opts
    (pPrint $ vsep [ text "Heap:"           <+> ppHeap h
                   , text "Symbolic Trace:" <+> listSpaced (map pretty t)
                   , text "Expression:"     <+> ppExp e
                   ])
    x

traceSym :: CEState -> a -> a
traceSym s = traceInfo (cesCCTOpts s)
  (pPrint $ text "Symbolic Trace:" <+> listSpaced (map pretty (reverse (cesTrace s))))

--- Concolic evaluation monad
data CEM a = CE { runCEM :: CEState -> Result (a, CEState) }

--- Monadic bind operation.
bindResult :: Result a -> (a -> Result b) -> Result b
bindResult (Return   x) f = f x
bindResult (Choice a b) f = Choice (bindResult a f) (bindResult b f)

instance Monad CEM where
  return x = CE $ \s -> Return (x, s)

  f >>= g = CE $ \s -> bindResult (runCEM f s) (\(x, s') -> runCEM (g x) s')

choice :: CEM a -> CEM a -> CEM a
choice x y = CE $ \s -> Choice (runCEM x s) (runCEM y s)

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
bindE :: VarIndex -> AExpr TypeExpr -> CEM ()
bindE v e = modifyHeap (bindExpr v e)

--- Bind a variable lazily to an expression
bindLE :: VarIndex -> AExpr TypeExpr -> CEM ()
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
bindArg :: AExpr TypeExpr -> CEM (AExpr TypeExpr)
bindArg e = case e of
  AVar      _ _ -> return e
  ALet  _ bs e' -> addBindings bs e' >>= bindArg
  AFree _ vs e' -> addFrees    vs e' >>= bindArg
  _              -> do v <- freshVar
                       bindE v e
                       return (AVar (annExpr e) v)

--- ----------------------------------------------------------------------------

--- Lookup the parameters and the rhs for a given function
lookupRule :: QName -> CEM (Maybe ([VarIndex], AExpr TypeExpr))
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

--- Store the argument variables of the flattened main expression
setMainArgs :: [VarIndex] -> CEM ()
setMainArgs vs = modify $ \s -> s { cesArgs = vs }

--- ----------------------------------------------------------------------------

--- Get the trace of symbolic information
getTrace :: CEM Trace
getTrace = gets cesTrace

--- Add a decision with symbolic information for case expression `cid` to the trace
mkDecision :: CaseID -> BranchNr -> VarIndex -> (QName, TypeExpr) -> [VarIndex]
           -> CEM ()
mkDecision cid bnr v cty args
  = modify (\s -> s { cesTrace = (Decision cid bnr v cty args) : cesTrace s })

--- ----------------------------------------------------------------------------

--- concolic evaluation
ceval :: CCTOpts -> [AFuncDecl TypeExpr] -> VarIndex -> AExpr TypeExpr
      -> (AExpr TypeExpr, [Trace], [VarIndex], VarIndex)
ceval opts fs v e = fromResult $ runCEM (flatten e >>= nf) (initState opts fs v)
  where
  --- Flatten a FlatCurry function call introducing fresh variables for
  --- its arguments
  --- Note: This function should only used to 'prepare' the main expression
  --- before concolic testing
  flatten :: AExpr TypeExpr -> CEM (AExpr TypeExpr)
  flatten exp = case exp of
    AComb ty FuncCall c es -> do
      vs <- mapM bindArg es
      setMainArgs (map varNr vs)
      return $ AComb ty FuncCall c vs
    _                      -> return exp

fromResult :: Result (AExpr TypeExpr, CEState)
           -> (AExpr TypeExpr, [Trace], [VarIndex], VarIndex)
fromResult (Return (e, s)) = traceSym s
  (e, [reverse $ cesTrace s], cesArgs s, cesFresh s)
fromResult (Choice  e1 e2) = let (r1, t1, args, v1) = fromResult e1
                                 (r2, t2, _   , v2) = fromResult e2
                             in (mkOr r1 r2, t1 ++ t2, args, min v1 v2)

--- Evaluate given FlatCurry expression to normal form
nf :: AExpr TypeExpr -> CEM (AExpr TypeExpr)
nf e = hnf e >>= \e' -> case e' of
  AComb ty ConsCall c es -> AComb ty ConsCall c <$> mapM nf es
  _                      -> return e'

--- Evaluate given FlatCurry expression to head normal form
hnf :: AExpr TypeExpr -> CEM (AExpr TypeExpr)
hnf exp = case exp of
  AVar        ty v -> traceStep exp $ hnfVar  ty v
  ALit        ty l -> traceStep exp $ hnfLit  ty l
  AComb ty ct f es -> traceStep exp $ hnfComb ty ct f es
  ALet      _ bs e -> traceStep exp $ hnfLet  bs e
  AFree     _ vs e -> traceStep exp $ hnfFree vs e
  AOr      _ e1 e2 -> traceStep exp $ hnfOr   e1 e2
  ACase  _ ct e bs -> traceStep exp $ hnfCase ct e bs
  ATyped     _ e _ -> traceStep exp $ hnf     e

--- Concolic evaluation of a variable
hnfVar :: TypeExpr -> VarIndex -> CEM (AExpr TypeExpr)
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
hnfLit :: TypeExpr -> Literal -> CEM (AExpr TypeExpr)
hnfLit ty l = return (ALit ty l)

--- Concolic evaluation of a combination:
---   * flattened function calls: check if function is builtin or user-defined
---                               and start corresponding concolic evaluation
---   * non-flattened function/constructor calls: flatten them
hnfComb :: TypeExpr -> CombType -> (QName, TypeExpr) -> [AExpr TypeExpr]
        -> CEM (AExpr TypeExpr)
hnfComb ty ct f es = case ct of
  FuncCall
    | all isVar es -> lookupRule (fst f) >>= \mrule -> case mrule of
      Nothing      -> ceBuiltin ty f es
      Just (xs, e) -> hnf (subst (mkSubst xs es) e)
    | otherwise    -> mvs >>= \vs -> hnf (AComb ty ct f vs)
  _                -> AComb ty ct f <$> mvs
 where
  mvs = mapM bindArg es

--- Concolic evaluation of a let expression
hnfLet :: [((VarIndex, TypeExpr), AExpr TypeExpr)] -> AExpr TypeExpr
       -> CEM (AExpr TypeExpr)
hnfLet bs e = case bs of
  [((i, _), e')]               -- let binding with case id: do not introduce a fresh variable!
    | i < 0 -> bindE i e'       >>  hnf e
  _         -> addBindings bs e >>= hnf

--- Add a list of local bindings to the heap
addBindings :: [((VarIndex, TypeExpr), AExpr TypeExpr)] -> AExpr TypeExpr
            -> CEM (AExpr TypeExpr)
addBindings bs e = do
  ys <- freshVars (length bs)
  let (txs, es) = unzip bs
      (xs, tys) = unzip txs
      sigma     = mkSubst xs (zipWith AVar tys ys)
  zipWithM_ bindE ys (map (subst sigma) es)
  return (subst sigma e)

--- Concolic evaluation of a declaration of free variables
hnfFree :: [(VarIndex, TypeExpr)] -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
hnfFree vs e = addFrees vs e >>= hnf

--- Add a list of free variables to the heap
addFrees :: [(VarIndex, TypeExpr)] -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
addFrees tvs e = do
  ys <- freshVars (length tvs)
  mapM_ bindF ys
  let (vs, tys) = unzip tvs
  return $ subst (mkSubst vs (zipWith AVar tys ys)) e

--- Concolic evaluation of a non-deterministic choice
hnfOr :: AExpr TypeExpr -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
hnfOr e1 e2 = hnf e1 <|> hnf e2

--- Concolic evaluation of a case expression
hnfCase :: CaseType -> AExpr TypeExpr -> [ABranchExpr TypeExpr] -> CEM (AExpr TypeExpr)
hnfCase ct e bs = do
  (cid, e')      <- rmvCaseID e
  ve@(AVar _ vi) <- bindArg e' -- flattening of scrutinized expression
  hnf ve >>= \v -> case v of
    ALit ty l -> case findBranch (ALPattern ty l) bs of
      Nothing          -> failS ty
      Just (_, _,  be) -> hnf be
    AComb ty ConsCall c es -> case findBranch (APattern ty c []) bs of
      Nothing          -> failS ty
      Just (n, vs, be) -> do
        mkDecision cid (BNr n bcnt) vi c (map varNr es)
        hnf (subst (mkSubst vs es) be)
    AComb _ FuncCall _ _
      | v == failedExpr ty -> failS ty
      where ty = annExpr v
    AVar ty i
      | ct == Rigid -> error "Suspended"
      | otherwise   -> narrowCase
      where
        narrowCase = foldr choice (failS ty) $ zipWith guess [1 ..] bs

        guess _ (ABranch (ALPattern  ty' l) be) = do
          bindE i (ALit ty' l)
          hnf be
        guess n (ABranch (APattern ty' c txs) be) = do
          ys  <- freshVars (length txs)
          mkDecision cid (BNr n bcnt) vi c ys
          let (xs, tys) = unzip txs
              es'       = zipWith AVar tys ys
          bindE i (AComb ty' ConsCall c es')
          hnf (subst (mkSubst xs es') be)
    _ -> error $ "Eval.hnfCase: " ++ show v
 where
  bcnt = length bs

--- Remove the case identifier from the scrutinized expression of a case expression
rmvCaseID :: AExpr TypeExpr -> CEM (VarIndex, AExpr TypeExpr)
rmvCaseID e = case e of
  AVar _ cid -> lookupBinding cid >>= \mbdg -> case mbdg of
               Just (BoundVar be) -> return (cid, be)
               _                  -> error $
                 "Eval.rmvCaseID: Unexpected binding " ++ show mbdg
  _          -> error $ "Eval.rmvCaseID: Expected case id but found " ++ show e

--- Failing evaluation
failS :: TypeExpr -> CEM (AExpr TypeExpr)
failS ty = return (failedExpr ty)

--- Evaluation to `True`
succeedS :: CEM (AExpr TypeExpr)
succeedS = return trueExpr

--- Evaluation which yields a free variable
unknownS :: TypeExpr -> CEM (AExpr TypeExpr)
unknownS ty = do
  v <- freshVar
  bindF v
  return (AVar ty v)

-- -----------------------------------------------------------------------------
-- Concolic evaluation of builtin functions
-- -----------------------------------------------------------------------------

ceBuiltin :: TypeExpr -> (QName, TypeExpr) -> [AExpr TypeExpr]
          -> CEM (AExpr TypeExpr)
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

unary :: (AExpr TypeExpr -> CEM (AExpr TypeExpr)) -> [AExpr TypeExpr]
      -> CEM (AExpr TypeExpr)
unary f es = case es of
  [e] -> f e
  _   -> error "Eval unary"

binary :: (AExpr TypeExpr -> AExpr TypeExpr -> CEM (AExpr TypeExpr))
       -> [AExpr TypeExpr] -> CEM (AExpr TypeExpr)
binary f es = case es of
  [e1,e2] -> f e1 e2
  _       -> error "Eval.binary"

--- Concolic evaluation of a higher order application
ceBuiltinApply :: AExpr TypeExpr -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinApply e1 e2 = hnf e1 >>= \v1 -> case v1 of
  AComb ty ct f es | isPartCall ct -> hnf $ addPartCallArg ty ct f es e2
  _                                -> ceBuiltinApply v1 e2

--- Concolic evaluation of a conditional expression `(&>)` / `cond`
ceBuiltinCond :: AExpr TypeExpr -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinCond e1 e2 = hnf e1 >>= \v1 -> case v1 of
  AComb _ FuncCall g [x ,y]
      -- replace `(e1 &> e2) &> e3` by `e1 &> (e2 &> e3)`
    | fst g == prel "&>"   -> hnf $ builtin cty1 "&>"   [x, builtin cty2 "&>" [y, e2]]
    | fst g == prel "cond" -> hnf $ builtin cty1 "cond" [x, builtin cty2 "cond" [y, e2]]
  _ | v1 == trueExpr       -> hnf e2
    | v1 == failedExpr ty  -> failS ty
    | otherwise            -> ceBuiltinCond v1 e2
 where
  cty1 = condType boolType
  cty2 = condType (annExpr e2)
  ty   = annExpr e2

--- Concolic evaluation of `ensureNotFree`
ceBuiltinEnsureNotFree :: AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinEnsureNotFree e = do
  v1 <- hnf e
  case v1 of
    AVar _ _ -> error "Eval.ensureNotFree: suspended"
    _        -> return v1

--- Concolic evaluation of `failed`
ceBuiltinFailed :: TypeExpr -> [AExpr TypeExpr] -> CEM (AExpr TypeExpr)
ceBuiltinFailed ty es = case es of
  [] -> failS ty
  _  -> error "Eval.ceBuiltinFailed"

--- Concolic evaluation of `error`
ceBuiltinError :: AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinError e = do
  v1 <- nf e
  case v1 of
    AComb _ ConsCall _ _ -> error $ fromFCY v1
    _                    -> ceBuiltinError v1

--- Concolic evaluation of `success`
ceBuiltinSuccess :: [AExpr TypeExpr] -> CEM (AExpr TypeExpr)
ceBuiltinSuccess es = case es of
  [] -> succeedS
  _  -> error "Eval.ceBuiltinSuccess"

--- Concolic evaluation of `unknown`
ceBuiltinUnknown :: TypeExpr -> [AExpr TypeExpr] -> CEM (AExpr TypeExpr)
ceBuiltinUnknown ty es = case es of
  [] -> unknownS ty
  _  -> error "Eval.ceBuiltinUnknown"

--- Concolic evaluation of `(?)`
ceBuiltinChoice :: AExpr TypeExpr -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinChoice = hnfOr

--- Concolic evaluation of a concurrent conjunction `(&)`
ceBuiltinAmp :: AExpr TypeExpr -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinAmp e1 e2 = hnf e1 >>= \v1 -> case v1 of
  AVar _ i -> lookupBinding i >>= \mbdg -> case mbdg of
    Just FreeVar                -> bindE i trueExpr >> hnf e2
    Just LazyFree               -> bindE i trueExpr >> hnf e2
    _                           -> other v1
  _ | v1 == trueExpr            -> hnf e2
    | v1 == failedExpr boolType -> failS boolType
    | otherwise                 -> other v1
 where
  other v1 = hnf e2 >>= \v2 -> let ty2 = annExpr v2 in case v2 of
    _ | v2 == trueExpr       -> hnf v1
      | v2 == failedExpr ty2 -> failS ty2
      | otherwise            -> ceBuiltinAmp v1 v2

--- Concolic evaluation of `negateFloat`
ceBuiltinNegFloat :: AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinNegFloat e = do
  v <- hnf e
  case v of
    ALit _ (Floatc l) -> return (toFCY (negate l))
    _                 -> ceBuiltinNegFloat v

--- Concolic evaluation of `ord`
ceBuiltinOrd :: AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinOrd e = hnf e >>= \v -> case v of
  ALit _ (Charc l) -> return (toFCY (ord l))
  _                -> ceBuiltinOrd v

--- Concolic evaluation of `chr`
ceBuiltinChr :: AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinChr e = hnf e >>= \v -> case v of
  ALit _ (Intc l) -> return (toFCY (chr l))
  _               -> ceBuiltinChr v

--- Concolic evaluation of `i2f`
ceBuiltinI2F :: AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinI2F e = do
  v <- hnf e
  case v of
    ALit _ (Intc l) -> return (toFCY (fromInteger l))
    _               -> ceBuiltinI2F v

--- Concolic evaluation of `(&&)`
ceBuiltinAnd :: AExpr TypeExpr -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinAnd e1 e2 = hnf $ ACase boolType Flex e1
  [ ABranch (APattern boolType (prel "False", boolType) []) falseExpr
  , ABranch (APattern boolType (prel "True",  boolType) []) e2
  ]

--- Concolic evaluation of `(||)`
ceBuiltinOr :: AExpr TypeExpr -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinOr e1 e2 = hnf $ ACase boolType Flex e1
  [ ABranch (APattern boolType (prel "True",  boolType) []) trueExpr
  , ABranch (APattern boolType (prel "False", boolType) []) e2
  ]

--- Concolic evaluation of `($!)` / `($!!)`
ceBuiltinDollarBangs :: (AExpr TypeExpr -> CEM (AExpr TypeExpr))
                     -> AExpr TypeExpr -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinDollarBangs f e1 e2 = f e2 >>= ceBuiltinApply e1

--- Concolic evaluation of `($##)`
ceBuiltinDollarHashHash :: AExpr TypeExpr -> AExpr TypeExpr
                        -> CEM (AExpr TypeExpr)
ceBuiltinDollarHashHash e1 e2 = do
  v2 <- nf e2
  case v2 of
    AVar _ _ -> ceBuiltinDollarHashHash e1 v2
    _        -> ceBuiltinApply e1 v2

--- Concolic evaluation of `(=:=)`
ceBuiltinUni :: AExpr TypeExpr -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
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
      | otherwise  -> failS boolType
    (AVar _ i, e@(AComb ty ConsCall c es))
      | occurCheck i e h -> failS boolType
      | otherwise        -> do
          js <- freshVars (length es)
          mapM_ bindF js
          let ys = zipWith AVar (map annExpr es) js
          bindE i (AComb ty ConsCall c ys)
          hnf $ combine (prel "&") (prel "=:=") trueExpr ys es
    (e@(AComb ty ConsCall c es), AVar _ j)
      | occurCheck j e h -> failS boolType
      | otherwise        -> do
          is <- freshVars (length es)
          mapM_ bindF is
          let ys = zipWith AVar (map annExpr es) is
          bindE j (AComb ty ConsCall c ys)
          hnf $ combine (prel "&") (prel "=:=") trueExpr es ys
    (AComb _ ConsCall c1 es1, AComb _ ConsCall c2 es2)
      | c1 == c2  -> hnf $ combine (prel "&") (prel "=:=") trueExpr es1 es2
      | otherwise -> failS boolType
    _ | all (== trueExpr) [v1, v2] -> succeedS
      | otherwise                  -> ceBuiltinUni v1 v2

--- Concolic evaluation of `(=:<=)`
ceBuiltinLazyUni :: AExpr TypeExpr -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinLazyUni e1 e2 = do
  v1 <- hnf e1
  case v1 of
    AVar ty i -> lookupBinding i >>= \mbdg -> case mbdg of
      Just FreeVar  -> bindLE i e2 >> succeedS
      Just LazyFree -> bindF i >> hnf (AComb boolType FuncCall (prel "=:=", unifyType ty) [v1, e2])
      _             -> ceBuiltinLazyUni v1 e2
    _     -> do
      v2 <- hnf e2
      case (v1, v2) of
        (ALit _ l1, ALit _ l2)
          | l1 == l2   -> succeedS
          | otherwise  -> failS boolType
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
          | otherwise -> failS boolType
        _             -> ceBuiltinUni v1 v2

--- Occur check for strict unification
occurCheck :: VarIndex -> AExpr TypeExpr -> Heap -> Bool
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
ceBuiltinIntOp :: ToFCY a => (Int -> Int -> a) -> AExpr TypeExpr
               -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinIntOp op e1 e2 = do
  v1 <- hnf e1
  v2 <- hnf e2
  case (v1, v2) of
    (ALit _ (Intc l1), ALit _ (Intc l2)) -> return $ toFCY (op l2 l1)
    _                                    -> ceBuiltinIntOp op v1 v2

--- Concolic evaluation of binary operations on characters (arithmetic, logical)
ceBuiltinCharOp :: ToFCY a => (Char -> Char -> a) -> AExpr TypeExpr
                -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinCharOp op e1 e2 = do
  v1 <- hnf e1
  v2 <- hnf e2
  case (v1, v2) of
    (ALit _ (Charc l1), ALit _ (Charc l2)) -> return $ toFCY (op l1 l2)
    _                                      -> ceBuiltinCharOp op v1 v2

--- Concolic evaluation of binary operations on floats (arithmetic, logical)
-- Note that PAKCS applies most binary operators in reverse order.
-- For this reason, the arguments of `ceBuiltinFloatOp` are switched internally.
ceBuiltinFloatOp :: ToFCY a => (Float -> Float -> a) -> AExpr TypeExpr
                 -> AExpr TypeExpr -> CEM (AExpr TypeExpr)
ceBuiltinFloatOp op e1 e2 = do
  v1 <- hnf e1
  v2 <- hnf e2
  case (v1, v2) of
    (ALit _ (Floatc l1), ALit _ (Floatc l2)) -> return $ toFCY (op l2 l1)
    _                                        -> ceBuiltinFloatOp op v1 v2

--- FlatCurry representation of a call to a builtin function
builtin :: TypeExpr -> String -> [AExpr TypeExpr] -> AExpr TypeExpr
builtin ty f es = AComb (resultType ty) FuncCall (prel f, ty) es
