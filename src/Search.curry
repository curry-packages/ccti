--- ----------------------------------------------------------------------------
--- This module includes the main search loop of the ccti.
---
--- @author  Jan Tikovsky
--- @version December 2017
--- ----------------------------------------------------------------------------
module Search where

import FiniteMap ( FM, addListToFM, addToFM_C, delFromFM, elemFM, emptyFM
                 , foldFM, lookupFM )

import FlatCurry.Annotated.Goodies (argTypes)
import FlatCurry.Annotated.Pretty  (ppExp)
import FlatCurry.Annotated.Types

import List (delete, intersect, union)

import Text.Pretty hiding          (compose)

import CCTOptions (CCTOpts (..), covScope)

import Data.PQ (PQ)

import FCY2SMTLib
import Eval                (CEState (..), ceval, fromSubst, initCEState, norm )
import FCYFunctorInstances
import FlatCurryGoodies    (TypeAnn, resArgTypes)
import Output

import Language.SMTLIB

import Search.CaseMap
import Search.Queue

import Solver.SMTLIB.Z3 ( SMTError (..), SMTOpts (..), SMTSess, defSMTOpts
                        , evalSessions, liftIOA, solveSMTVars, z3)

import Substitution (AExpSubst, compose, dom, mkSubst, restrict, subst)
import Symbolic
import Utils        (fst3, mapM_, unlessM, ppFM, whenM)

--- Map of unvisited symbolic nodes, i.e. case branches
type CoverMap = ContextMap Context CoverInfo

--- Represent symbolic execution tree by priority queue
type SymTree = PQ Depth SymNode

--- Test case data: function call with arguments and corresponding
--- non-deterministic results
type TestCase = (AExpr TypeExpr, [AExpr TypeExpr])

--- Information on SMT solver failures
data SMTFail = SMTFail [SMTError] [VarIndex] [Command]

--- Pretty printing of test cases
ppTestCase :: (AExpr TypeExpr, [AExpr TypeExpr]) -> Doc
ppTestCase (e, res) = parens (ppExp e <+> equals <+> set (map ppExp res))

data CSState = CSState
  { cssCCTOpts  :: CCTOpts
  , cssSMTInfo  :: SMTInfo
  , cssTree     :: SymTree
  , cssCoverMap :: CoverMap
  , cssTests    :: [TestCase]
  , cssSymArgs  :: [VarIndex]
  , cssSMTFails :: [SMTFail]
  }

--- Initial state for the concolic search
initCSState :: CCTOpts -> SMTInfo -> [VarIndex] -> CSState
initCSState opts smtInfo sargs = CSState
  { cssCCTOpts  = opts
  , cssSMTInfo  = smtInfo
  , cssTree     = emptySQ
  , cssCoverMap = emptyCM
  , cssTests    = []
  , cssSymArgs  = sargs
  , cssSMTFails = []
  }

--------------------------------------------------------------------------------

-- TODO: replace with StateT CSState IO a when state transformers are available
--- Concolic search monad
data CSM a = CS { runCSM :: CSState -> SMTSess (a, CSState) }

instance Monad CSM where
  return x = CS $ \s -> return (x, s)

  m >>= f = CS $ \s -> runCSM m s >>= \(x, s') -> runCSM (f x) s'

execCSM :: CSM a -> CSState -> SMTSess CSState
execCSM m s = runCSM m s >>= return . snd

gets :: (CSState -> a) -> CSM a
gets f = CS $ \s -> return (f s, s)

get :: CSM CSState
get = gets id

put :: CSState -> CSM ()
put s = CS $ \_ -> return ((), s)

modify :: (CSState -> CSState) -> CSM ()
modify f = CS $ \s -> return ((), f s)

--- Lift SMT sessions to CSM
liftSMTSess :: SMTSess a -> CSM a
liftSMTSess sess = CS $ \s -> sess >>= \x -> return (x, s)

--- Lift IO actions to CSM
io :: IO a -> CSM a
io = liftSMTSess . liftIOA

--------------------------------------------------------------------------------

--- Get concolic search options
getOpts :: CSM CCTOpts
getOpts = gets cssCCTOpts

--- Get current symbolic tree
getSymTree :: CSM SymTree
getSymTree = gets cssTree

setSymTree :: SymTree -> CSM ()
setSymTree st = modify $ \s -> s { cssTree = st }

--- Get current map of unvisited branches
getCoverMap :: CSM CoverMap
getCoverMap = gets cssCoverMap

--- Get SMTInfo object
getSMTInfo :: CSM SMTInfo
getSMTInfo = gets cssSMTInfo

--- Get symbolic arguments of expression to be tested
getSymArgs :: CSM [VarIndex]
getSymArgs = gets cssSymArgs

--- Get current test suite
getTestCases :: CSM [TestCase]
getTestCases = gets cssTests

--- Add a test case to the current test suite
--- Return 'True' if its a new test case and 'False' otherwise
addTestCase :: TestCase -> CSM Bool
addTestCase tc = do
  tcs <- getTestCases
  if tc `elem` tcs
    then return False
    else modify (\s -> s { cssTests = tc : tcs }) >> return True

--- Add information on SMT solver failure
addSMTFail :: [SMTError] -> [VarIndex] -> [Command] -> CSM ()
addSMTFail errs vars cmds =
  modify $ \s -> s { cssSMTFails = SMTFail errs vars cmds : cssSMTFails s }

--- Get the constructors / constraints already covered for a given context
getCovered :: Context -> CSM CoveredCs
getCovered ctxt = getCoverMap >>= return . covered ctxt

--------------------------------------------------------------------------------

type UpdSymInfo = SymTree -> CoverMap -> SMTInfo -> (SymTree, CoverMap, SMTInfo)

updSymInfo :: Trace -> CSM ()
updSymInfo t = modify $ \s ->
  let (st, uvn, te) = processTrace (cssCCTOpts s) t (cssTree s) (cssCoverMap s) (cssSMTInfo s)
  in s { cssSMTInfo = te, cssTree = st, cssCoverMap = uvn }

processTrace :: CCTOpts -> Trace -> UpdSymInfo
processTrace opts trace tree uvNodes smtInfo
  = prcTrace trace 0 [] [] [] [] (covScope opts) tree uvNodes smtInfo
 where
  prcTrace []                                  _ _     _  _  _    _  st uvn smtEnv = (st, uvn, smtEnv)
  prcTrace (Decision cid bnr v sobj args : ds) d cidcs cs vs ctxt cl st uvn smtEnv =
    let -- extended list of required SMT-LIB constant indices
        cidcs'  = cidcs `union` (v : args)
        -- extended list of known variables
        vs'     = v : vs
        -- extend context information (history of case ids along the execution path)
        -- the context history is limited by the maximal context length
        ctxt'   = take cl (cid : ctxt)
        -- extended symbolic tree
        st'     = enqueueSQ d (SymNode d ctxt' cidcs' cs vs v) st
        -- updated type environment
        smtEnv' = execSMTTrans (updTypeEnv (v:args) (soType sobj) args) smtEnv
        -- generate constraint information
        ci      = genCsInfo smtEnv' sobj v args
        -- extended list of path constraints
        cs'     = cs ++ [genPConstr ci v args]
        -- cover traced branch
        uvn'    = cover ctxt' bnr ci uvn
    in prcTrace ds (d+1) cidcs' cs' vs' ctxt' cl st' uvn' smtEnv'

--- Generate constraint information
genCsInfo :: SMTInfo -> SymObj -> VarIndex -> [VarIndex] -> CoveredCs
genCsInfo smtInfo sobj v args = case sobj of
  SymCons  _ _ -> CCons [(cons2SMT smtInfo sobj v, map (flip getSMTSort smtInfo) args)]
  SymLit lcs l -> CConstr (\x -> (lcs2SMT lcs) x (lit2SMT l))

lcs2SMT :: LConstr -> Term -> Term -> Term
lcs2SMT E  = (=%)
lcs2SMT NE = (/=%)
lcs2SMT L  = (<%)
lcs2SMT LE = (<=%)
lcs2SMT G  = (>%)
lcs2SMT GE = (>=%)

--- Generate a path constraint
genPConstr :: CoveredCs -> VarIndex -> [VarIndex] -> Term
genPConstr (CCons cons) v args = case cons of
  [(qi, _)] -> tvar v =% qtcomb qi (map tvar args)
  _         -> error $ "Search.genPConstr: Invalid constraint info"
genPConstr (CConstr constr) v _    = constr (tvar v)

csearch :: CCTOpts -> [AFuncDecl TypeAnn] -> VarIndex -> SMTInfo
        -> AExpr TypeAnn -> IO [TestCase]
csearch opts fs v smtInfo e = do
  -- prepare main expression for concolic search
  let (sub, e', v') = norm v e
      ceState       = initCEState opts sub fs v'
      smtOpts       = defSMTOpts { globalCmds = smtDecls smtInfo
                                 , quiet      = not $ optDebugSearch opts
                                 , tracing    = optDumpSMT opts
                                 }
  s <- evalSessions z3 smtOpts { globalCmds = smtDecls smtInfo } $
         execCSM (searchLoop sub ceState e')
                 (initCSState opts smtInfo (dom sub))
  let fails = cssSMTFails s
  unlessM (null fails) $ do
    putStrLn $ show (length fails) ++ " failure(s) occurred during solving."
    putStrLn $ "Dumping corresponding SMT-LIB scripts to .smt/smtFail<timestamp>.smt2"
    writeSMTDump "smtFail" (foldr genSMTFail (smtDecls smtInfo) fails)
  return (cssTests s)

searchLoop :: AExpSubst -> CEState -> AExpr TypeAnn -> CSM ()
searchLoop = searchLoopN 20 (return ())

--- main loop of concolic search
searchLoopN :: Int -> CSM () -> AExpSubst -> CEState -> AExpr TypeAnn -> CSM ()
searchLoopN d skipNode phi ceState e
  | d == 0    = return ()
  | otherwise = do
  opts <- getOpts
  -- start concolic evalutation
  -- we need to rename trace variables due to possible naming conflicts
  -- in non-deterministic branches
  (rs, ts, v') <- renameTraces (ceval e ceState)
  -- TODO: Simplify conversion
  let tcase = (fmap fst3 (subst phi e), map (fmap fst3) rs)
  io $ debugSearch opts $ "New test case: " ++ pPrint (ppTestCase tcase)
  isNewTest <- addTestCase tcase
  -- in case no new test data was generated skip current symbolic node
  unlessM isNewTest skipNode
  io $ debugSearch opts $
    "Symbolic Traces: " ++ pPrint (listSpaced (map ppTrace ts))
  mapM_ updSymInfo ts
  st  <- getSymTree
  io $ debugSearch opts $ "Priority Queue: " ++ pPrint (pretty st)
  uv <- getCoverMap
  io $ debugSearch opts $ "CoverMap: " ++ pPrint
    (ppFM (\(cid,m) -> ppFM (\(_,n) -> int cid <+>  text (show n)) m) (cm uv))
  nxt <- nextSymNode
  case nxt of
    Nothing -> return ()
    Just  n -> do
      io $ debugSearch opts $ "Next node: " ++ show n
      cmds <- genSMTCmds v' n
      let is = getSMTArgs n phi
      answer <- liftSMTSess $ solveSMTVars (map tvar is) cmds
      case answer of
        -- collect any SMT solver related failures and continue with search
        Left errs -> do
          addSMTFail errs is cmds
          rmvSymNode
          searchLoopN (d-1) (skipSymNode n) phi ceState { cesFresh = v', cesHeap = fromSubst phi } e
        Right vps -> do
          smtInfo <- getSMTInfo
          let sigma = mkSubst is $ zipWith (fromTerm smtInfo) is (map snd vps)
              theta = phi `compose` sigma
          searchLoopN (d-1) (skipSymNode n) theta ceState { cesFresh = v', cesHeap = fromSubst theta } e

--- Renaming of trace variables
renameTraces :: ([AExpr TypeAnn], [Trace], VarIndex)
             -> CSM ([AExpr TypeAnn], [Trace], VarIndex)
renameTraces info@(res, traces, v)
  | length traces < 2 = return info
  | otherwise         = do
    sargs <- getSymArgs
    let (t:ts)    = traces
        (ts', v') = foldr (rnmTrace sargs) ([], v) ts
    return (res, t : reverse ts', v')

-- TODO: Overthink abstractions for symbolic tree
-- this method should be provided by the interface of symbolic tree
--- Get next symbolic node from search tree if there is still one left
--- abort criterion
nextSymNode :: CSM (Maybe SymNode)
nextSymNode = do
  opts <- getOpts
  uvn  <- getCoverMap
  st   <- getSymTree
  case dequeueSQ st of
    Nothing                        -> return Nothing
    Just (n@(SymNode d ctxt _ _ _ _), st')
      | d > optSearchDepth opts    -> return Nothing
      | isCovered ctxt uvn         -> setSymTree st' >> nextSymNode
      | otherwise                  -> return (Just n)

--- Remove next node from symbolic tree
rmvSymNode :: CSM ()
rmvSymNode = do
  st <- getSymTree
  case dequeueSQ st of
    Nothing       -> return ()
    Just (_, st') -> setSymTree st'

--- Generate SMT-LIB commands for the variable declarations
--- and the assertion of path constraints for the given symbolic node
genSMTCmds :: VarIndex -> SymNode -> CSM [Command]
genSMTCmds v (SymNode _ ctxt cidcs pcs _ dv) = do
  smtInfo <- getSMTInfo
  ci      <- getCovered ctxt
  let pc = case ci of
             CCons   cons   -> noneOf v dv cons
             CConstr constr -> [tneg (constr (tvar dv))]
  return $ declConsts smtInfo cidcs ++ [assert (pcs ++ pc)]

--- Get variable indices for arguments of concolically tested expression
getSMTArgs :: SymNode -> AExpSubst -> [VarIndex]
getSMTArgs (SymNode _ _ _ _ vs v) = dom . restrict (v:vs)

--- Generate SMT commands from SMTFails
genSMTFail :: SMTFail -> [Command] -> [Command]
genSMTFail (SMTFail errs vs cmds) css = concat
  [ css, map (comment . show) errs, (Push 1 : cmds)
  , [CheckSat, GetValue (map tvar vs), Pop 1]
  ]

--- Skip a node in the symbolic execution tree
--- by marking all its branches as covered
skipSymNode :: SymNode -> CSM ()
skipSymNode (SymNode _ ctxt _ _ _ _) =
  modify $ \s -> s { cssCoverMap = coverAll ctxt (cssCoverMap s) }
