module Search where

import FiniteMap                 ( FM, addListToFM, addToFM_C, delFromFM, elemFM
                                 , emptyFM, foldFM, lookupFM )
import FlatCurry.Annotated.Goodies (argTypes)
import FlatCurry.Annotated.Types
import List                      (delete, intersect, union)

import CCTOptions                (CCTOpts (..))
import FCY2SMTLib
import Eval                      ( CEState (..), ceval, fromSubst, initCEState
                                 , norm )
import FlatCurryGoodies          (SymObj (..), resArgTypes, tyOf)
import Heap                      (toSubst)
import Output
import PrettyPrint hiding        (compose)
import Search.DFS
import SMTLib.Goodies
import SMTLib.Pretty             (showSMT)
import SMTLib.Solver
import SMTLib.Types              ( Command (..), QIdent, Sort (..), SMTLib (..)
                                 , Sort, Term )
import Substitution              ( AExpSubst, compose, dom, mkSubst, restrict
                                 , subst)
import Symbolic                  ( BranchNr (..), CaseID, Decision (..), Depth
                                 , SymNode (..), Trace, ppTrace, rnmTrace )
import Utils                     (mapM, mapM_, unlessM, whenM)

--- Map of unvisited symbolic nodes, i.e. case branches
type UVNodes = FM CaseID CaseInfo

--- Information on case expressions required for the concolic search, namely:
---   * numbers of unvisited branches
---   * known constructor (qualified identifier and argument sorts)
data CaseInfo = CaseInfo
  { ubs    :: [Int]
  , csInfo :: ConstrInfo
  }
  deriving Show

--- Information for the generation of path constraints, i.e.
---   * known constructors
---   * literal constraint
data ConstrInfo = KnownCons [(QIdent, [Sort])]
                | LitConstr (Term -> Term)

instance Show ConstrInfo where
  show (KnownCons   cons) = "KnownCons " ++ show cons
  show (LitConstr constr) = "LitConstr " ++ show (constr (tvar 0))

--- Test case data: function call with arguments and corresponding
--- non-deterministic results
type TestCase = (AExpr TypeExpr, [AExpr TypeExpr])

data CSState = CSState
  { cssCCTOpts :: CCTOpts
  , cssSMTInfo :: SMTInfo
  , cssTree    :: SymTree
  , cssUVNodes :: UVNodes
  , cssSession :: SolverSession
  , cssTests   :: [TestCase]
  , cssSymArgs :: [VarIndex]
  }

--- Initial state for the concolic search
initCSState :: CCTOpts -> SMTInfo -> SolverSession -> [VarIndex] -> CSState
initCSState opts smtInfo session sargs = CSState
  { cssCCTOpts = opts
  , cssSMTInfo = smtInfo
  , cssTree    = newTree
  , cssUVNodes = emptyFM (<)
  , cssSession = session
  , cssTests   = []
  , cssSymArgs = sargs
  }

--------------------------------------------------------------------------------

-- TODO: replace with StateT CSState IO a when state transformers are available
--- Concolic search monad
data CSM a = CS { runCSM :: CSState -> IO (a, CSState) }

instance Monad CSM where
  return x = CS $ \s -> return (x, s)

  m >>= f = CS $ \s -> runCSM m s >>= \(x, s') -> runCSM (f x) s'

execCSM :: CSM a -> CSState -> IO CSState
execCSM m s = runCSM m s >>= return . snd

gets :: (CSState -> a) -> CSM a
gets f = CS $ \s -> return (f s, s)

get :: CSM CSState
get = gets id

put :: CSState -> CSM ()
put s = CS $ \_ -> return ((), s)

modify :: (CSState -> CSState) -> CSM ()
modify f = CS $ \s -> return ((), f s)

--- Lift IO actions to CSM
io :: IO a -> CSM a
io m = CS $ \s -> m >>= \x -> return (x, s)

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
getUVNodes :: CSM UVNodes
getUVNodes = gets cssUVNodes

--- Get current solver session
getSession :: CSM SolverSession
getSession = gets cssSession

--- Get SMTInfo object
getSMTInfo :: CSM SMTInfo
getSMTInfo = gets cssSMTInfo

--- Get symbolic arguments of expression to be tested
getSymArgs :: CSM [VarIndex]
getSymArgs = gets cssSymArgs

--- Add a test case to the current test cases
addTestCase :: TestCase -> CSM ()
addTestCase tc = modify $ \s -> s { cssTests = [tc] `union` cssTests s }

--- Get the constraint information for the given case identifier
getConstrInfo :: CaseID -> CSM ConstrInfo
getConstrInfo cid = do
  uvn <- getUVNodes
  case lookupFM uvn cid of
    Nothing                  -> error $
      "Search.lookupCsInfo: No constraint information for " ++ show cid
    Just ci -> return (csInfo ci)

--- Get the number of unvisited nodes
numOfUnvis :: UVNodes -> Int
numOfUnvis = foldFM (\_ ci acc -> (length (ubs ci) + acc)) 0

--------------------------------------------------------------------------------

type UpdSymInfo = SymTree -> UVNodes -> SMTInfo -> (SymTree, UVNodes, SMTInfo)

--- Prepare next iteration of search
prepSearch :: TestCase -> [Trace] -> CSM ()
prepSearch tcase ts = do
  uvn  <- getUVNodes
  mapM_ updSymInfo ts
  uvn' <- getUVNodes
  if numOfUnvis uvn' < numOfUnvis uvn || numOfUnvis uvn == 0
    then addTestCase tcase
    else rmvSymNode

updSymInfo :: Trace -> CSM ()
updSymInfo t = modify $ \s ->
  let (st, uvn, te) = processTrace t (cssTree s) (cssUVNodes s) (cssSMTInfo s)
  in s { cssSMTInfo = te, cssTree = st, cssUVNodes = uvn }

processTrace :: Trace -> UpdSymInfo
processTrace trace tree uvNodes smtInfo
  = prcTrace trace 0 [] [] [] tree uvNodes smtInfo
  where
  prcTrace []                                  _ _     _  _  st uvn smtEnv = (st, uvn, smtEnv)
  prcTrace (Decision cid bnr v sobj args : ds) d cidcs cs vs st uvn smtEnv =
    let -- extended list of required SMT-LIB constant indices
        cidcs'  = cidcs `union` (v : args)
        -- extended list of known variables
        vs'     = v : vs
        -- extended symbolic tree
        st'     = addNode (SymNode d cid cidcs' cs vs v) st
        -- updated type environment
        smtEnv' = execSMTTrans (updTypeEnv (v:args) (tyOf sobj) args) smtEnv
        -- generate constraint information
        ci      = genCsInfo smtEnv' sobj v args
        -- extended list of path constraints
        cs'     = cs ++ [genPConstr ci v args]
        -- updated unvisited nodes
        uvn'    = visitBranch cid bnr ci uvn
    in prcTrace ds (d+1) cidcs' cs' vs' st' uvn' smtEnv'

--- Generate constraint information
genCsInfo :: SMTInfo -> SymObj -> VarIndex -> [VarIndex] -> ConstrInfo
genCsInfo smtInfo sobj v args = case sobj of
  SymCons  _ _ -> KnownCons [(cons2SMT smtInfo sobj v, map (flip getSMTSort smtInfo) args)]
  SymLit lcs l -> LitConstr (\x -> (lcs2SMT lcs) x (lit2SMT l))

--- Generate a path constraint
genPConstr :: ConstrInfo -> VarIndex -> [VarIndex] -> Term
genPConstr (KnownCons   cons) v args = case cons of
  [(qi, _)] -> tvar v =% qtcomb qi (map tvar args)
  _         -> error $ "Search.genPConstr: Invalid constraint info"
genPConstr (LitConstr constr) v _    = constr (tvar v)

--- Mark a branch as visited during concolic search
--- by updating the map of unvisited nodes
visitBranch :: CaseID -> BranchNr -> ConstrInfo -> UVNodes -> UVNodes
visitBranch cid (BNr m n) ci uvn
  = addToFM_C updCaseInfo uvn cid (CaseInfo [b | b <- [1 .. n], b /= m] ci)
  where
  updCaseInfo (CaseInfo bs1 old) (CaseInfo bs2 new)
    = CaseInfo (bs1 `intersect` bs2) (combine old new)
    where
    combine ci1 ci2 = case (ci1, ci2) of
      (KnownCons cons1, KnownCons cons2) -> KnownCons (cons1 `union` cons2)
      _                                  -> ci1

csearch :: CCTOpts -> [AFuncDecl TypeExpr] -> VarIndex -> SMTInfo
        -> AExpr TypeExpr -> IO [TestCase]
csearch opts fs v smtInfo e = do
  -- prepare main expression for concolic search
  let (sub, e', v') = norm v e
      ceState       = initCEState opts sub fs v'
  -- initialize solver session
  status opts "Initializing solver session"
  session <- initSession z3 (smtDecls smtInfo)
  s       <- execCSM (searchLoop sub ceState e')
                     (initCSState opts smtInfo session (dom sub))
  -- terminate solver session
  termSession (cssSession s)
  -- dump file with SMT-LIB commands
  dumpSMT opts $ showSMT $ trace $ cssSession s
  return (cssTests s)

searchLoop :: AExpSubst -> CEState -> AExpr TypeExpr -> CSM ()
searchLoop = searchLoopN 10

--- main loop of concolic search
searchLoopN :: Int -> AExpSubst -> CEState -> AExpr TypeExpr -> CSM ()
searchLoopN d sub ceState e
  | d == 0    = return ()
  | otherwise = do
  opts <- getOpts
  -- start concolic evalutation
  -- we need to rename trace variables due to possible naming conflicts
  -- in non-deterministic branches
  (rs, ts, v') <- renameTraces (ceval e ceState)
  let tcase = (subst sub e, rs)
  io $ debugSearch opts $ "New test case: " ++ pPrint (ppTestCase tcase)
  addTestCase tcase
  io $ debugSearch opts $
    "Symbolic Traces: " ++ pPrint (listSpaced (map ppTrace ts))
  mapM_ updSymInfo ts
--   prepSearch tcase ts
  st  <- getSymTree
  io $ debugSearch opts $ "Priority Queue: " ++ pPrint (ppFM (\(_,n) -> text (show n)) st)
  uv <- getUVNodes
  io $ debugSearch opts $ "Case Map: " ++ pPrint (ppFM (\(cid,n) -> int cid <+> text (show n)) uv)
  nxt <- nextSymNode
  case nxt of
    Nothing -> return ()
    Just  n -> do
      io $ debugSearch opts $ "Next node: " ++ show n
      cmds    <- genSMTCmds v' n
      msub    <- solve (getSMTArgs n sub) cmds
      case msub of
        Nothing   -> return () -- instead of complete abort, drop node and continue search?
        Just bdgs -> do
          let sub'     = sub `compose` bdgs
              ceState' = ceState { cesFresh = v'
                                 , cesHeap  = fromSubst opts sub'
                                 }
          unlessM (optIncremental opts) (csm (restartSession z3))
          searchLoopN (d-1) sub' ceState' e

--- Renaming of trace variables
renameTraces :: ([AExpr TypeExpr], [Trace], VarIndex)
             -> CSM ([AExpr TypeExpr], [Trace], VarIndex)
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
  uvn  <- getUVNodes
  st   <- getSymTree
  case nextNode st of
    Nothing                        -> return Nothing
    Just n@(SymNode d cid _ _ _ _)
      | d > optSearchDepth opts    -> return Nothing
      | hasUnvis cid uvn           -> return (Just n)
      | otherwise                  -> setSymTree (delNode d st) >> nextSymNode

--- Remove next node from symbolic tree
rmvSymNode :: CSM ()
rmvSymNode = do
  mnode <- nextSymNode
  st    <- getSymTree
  case mnode of
    Nothing -> return ()
    Just (SymNode d _ _ _ _ _) -> setSymTree (delNode d st)

hasUnvis :: CaseID -> UVNodes -> Bool
hasUnvis cid uv = case lookupFM uv cid of
  Nothing -> False
  Just ci -> not (null (ubs ci))

--- Generate SMT-LIB commands for the variable declarations
--- and the assertion of path constraints for the given symbolic node
genSMTCmds :: VarIndex -> SymNode -> CSM [Command]
genSMTCmds v (SymNode _ cid cidcs pcs _ dv) = do
  smtInfo <- getSMTInfo
  ci      <- getConstrInfo cid
  let pc = case ci of
             KnownCons cons   -> noneOf v dv cons
             LitConstr constr -> [tneg (constr (tvar dv))]
  return $ declConsts smtInfo cidcs ++ [assert (pcs ++ pc)]

--- Select subset of argument variables of concolically tested expression
getSMTArgs :: SymNode -> AExpSubst -> [VarIndex]
getSMTArgs (SymNode _ _ _ _ vs v) = dom . restrict (v:vs)

--- Compute bindings for the arguments of the concolically tested expression
--- in form of a substitution by running an SMT solver
solve :: [VarIndex] -> [Command] -> CSM (Maybe AExpSubst)
solve vs cmds = do
  opts    <- getOpts
  smtInfo <- getSMTInfo
  -- remove constraints from previous iteration
  whenM (optIncremental opts) (csm resetStack)
  -- add constraints to solver stack
  csm $ sendCmds cmds
  io  $ debugSearch opts $ "SMT-LIB model:\n" ++ showSMT cmds
  -- check satisfiability
  isSat <- csm checkSat
  io $ debugSearch opts $ "Check satisfiability: " ++ pPrint (pretty isSat)
  case isSat of
    Sat -> do
      vals <- csm $ getValues (map tvar vs)
      io $ debugSearch opts $ "Get values: " ++ pPrint (pretty vals)
      case vals of
        Values vps -> return $ Just $ mkSubst vs $
                        zipWith (fromTerm smtInfo) vs (map snd vps)
        _          -> return Nothing
    _   -> return Nothing

-- helper

--- Lift an SMT solver operation to CSM
csm :: SMTOp a -> CSM a
csm computation = do
  s              <- get
  (res, session) <- io $ computation (cssSession s)
  put s { cssSession = session }
  return res
