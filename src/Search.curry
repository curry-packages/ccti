module Search where

import FiniteMap                 ( FM, addListToFM, addToFM, delFromFM, elemFM
                                 , emptyFM, lookupFM )
import FlatCurry.Annotated.Types
import List                      (delete, union)

import CCTOptions                (CCTOpts (..))
import FCY2SMTLib
import Eval                      (ceval, flatten)
import FlatCurryGoodies          (TypedFCYCons (..), getTypes)
import Output
import PrettyPrint hiding        (compose)
import Search.DFS
import SMTLib.Goodies            ((=%), assert, noneOf, tvar, var2SMT)
import SMTLib.Pretty             (showSMT)
import SMTLib.Solver
import SMTLib.Types              (Command (..), Sort (..), SMTLib (..), Sort, Term)
import Substitution              ( AExpSubst, compose, dom, mkSubst, restrict
                                 , substExp)
import Symbolic                  ( BranchNr (..), CaseID, Decision (..), Depth
                                 , SymNode (..), Trace )
import Utils                     (mapM_)

--- Map of unvisited symbolic nodes, i.e. case branches
type UVNodes = FM CaseID CaseInfo

--- Information on case expressions required for the concolic search, namely:
---   * numbers of unvisited branches
---   * known constructor terms (in SMT-LIB representation)
---   * variable declarations (in SMT-LIB representation)
data CaseInfo = CaseInfo [Int] [Term] [Command]
  deriving Show

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
  }

--- Initial state for the concolic search
initCSState :: CCTOpts -> SMTInfo -> SolverSession -> CSState
initCSState opts smtInfo session = CSState
  { cssCCTOpts = opts
  , cssSMTInfo = smtInfo
  , cssTree    = newTree
  , cssUVNodes = emptyFM (<)
  , cssSession = session
  , cssTests   = []
  }

--------------------------------------------------------------------------------

-- TODO: replace with StateT CSState IO a when state transformers are available
--- Concolic search monad
data CSM a = CS { runCSM :: CSState -> IO (a, CSState) }

instance Monad CSM where
  return x = CS $ \s -> return (x, s)

  m >>= f = CS $ \s -> runCSM m s >>= \(x, s') -> runCSM (f x) s'

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

--- Get current map of unvisited branches
getUVNodes :: CSM UVNodes
getUVNodes = gets cssUVNodes

--- Get current solver session
getSession :: CSM SolverSession
getSession = gets cssSession

--- Get SMTInfo object
getSMTInfo :: CSM SMTInfo
getSMTInfo = gets cssSMTInfo

--- Add a test case to the current test cases
addTestCase :: TestCase -> CSM ()
addTestCase tc = modify $ \s -> s { cssTests = tc : cssTests s }

--------------------------------------------------------------------------------

type UpdSymInfo = SymTree -> UVNodes -> SMTInfo -> (SymTree, UVNodes, SMTInfo)

updSymInfo :: Trace -> CSM ()
updSymInfo t = modify $ \s ->
  let (st, uvn, te) = processTrace t (cssTree s) (cssUVNodes s) (cssSMTInfo s)
  in s { cssSMTInfo = te, cssTree = st, cssUVNodes = uvn }

processTrace :: Trace -> UpdSymInfo
processTrace trace tree uvNodes smtInfo
  = prcTrace trace 0 [] [] [] tree uvNodes smtInfo
  where
  prcTrace []                                                _ _   _  _  st uvn smtEnv = (st, uvn, smtEnv)
  prcTrace (Decision cid bnr v c@(TFCYCons _ cty) args : ds) d vds cs vs st uvn smtEnv =
        -- SMT-LIB term representation of selected FlatCurry constructor
    let t     = toTerm smtEnv c args
        -- type information for SMT variables
        vtys    = zip (args ++ [v])
                      (zipWith (newTypeInfo smtEnv) (getTypes cty)
                                                     (args : repeat []))
        -- additional SMT-LIB variable declarations required for this node
        vdecs   = map (uncurry declConst) vtys
        -- extended SMT-LIB variable declarations
        vds'    = vds `union` vdecs
        -- extended list of path constraints
        cs'     = cs ++ [tvar v =% t]
        -- extended list of known variables
        vs'     = v : vs
        -- extended symbolic tree
        st'     = addNode (SymNode d cid vds cs vs v) st
        -- updated unvisited nodes
        uvn'    = visitBranch cid bnr t vdecs uvn
        -- updated type environment
        smtEnv' = smtEnv { smtVars = addListToFM (smtVars smtEnv) vtys }
    in prcTrace ds (d+1) vds' cs' vs' st' uvn' smtEnv'
  -- specialize type information
--   specialize old new = case old of
--     (TypeInfo (TVar _) _) -> new
--     _                     -> old

--- Mark a branch as visited during concolic search
--- by updating the map of unvisited nodes
visitBranch :: CaseID -> BranchNr -> Term -> [Command] -> UVNodes -> UVNodes
visitBranch cid (BNr m n) t vds uvn = case lookupFM uvn cid of
  Nothing
    | n == 1    -> uvn
    | otherwise -> addToFM uvn cid
                     (CaseInfo [b | b <- [1 .. n], b /= m] [t] vds)
  Just (CaseInfo bs cons decls)
    | null bs'  -> delFromFM uvn cid
    | otherwise -> addToFM uvn cid
                     (CaseInfo bs' ([t] `union` cons) (vds `union` decls))
    where
    bs' = delete m bs

csearch :: CCTOpts -> [AFuncDecl TypeExpr] -> VarIndex -> SMTInfo
        -> AExpr TypeExpr -> IO [TestCase]
csearch opts fs v smtInfo e = do
  -- prepare main expression for concolic search
  let (sub, e') = flatten v e
  -- initialize solver session
  status opts "Initializing solver session"
  session <- initSession z3 smtInfo
  (_, s) <- runCSM (searchLoop fs v sub e') (initCSState opts smtInfo session)
  -- terminate solver session
  termSession (cssSession s)
  -- dump file with SMT-LIB commands
  dumpSMT opts $ showSMT $ trace $ cssSession s
  return (cssTests s)

--- main loop of concolic search
searchLoop :: [AFuncDecl TypeExpr] -> VarIndex -> AExpSubst -> AExpr TypeExpr
           -> CSM ()
searchLoop fs v sub sexp = do
  opts <- getOpts
  -- apply substitution on symbolic expression
  let cexp = substExp sub sexp
      (res, ts, v') = ceval opts fs v cexp
      tcase = (cexp, res)
  io $ debugSearch opts $ "Found test case: " ++ pPrint (ppTestCase tcase)
  addTestCase tcase
  mapM_ updSymInfo ts
  st  <- getSymTree
  io $ debugSearch opts $ "Priority Queue: " ++ pPrint (ppFM (\(_,n) -> text (show n)) st)
  nxt <- nextSymNode st
  case nxt of
    Nothing -> return ()
    Just  n -> do
      io $ debugSearch opts $ "Next node: " ++ show n
      cmds    <- genSMTCmds n
      msub    <- solve (getSMTArgs n sub) cmds
      case msub of
        Nothing   -> return () -- instead of complete abort, drop node and continue search?
        Just sub' -> searchLoop fs v (sub `compose` sub') sexp

-- TODO: Overthink abstractions for symbolic tree
-- this method should be provided by the interface of symbolic tree
--- Get next symbolic node from search tree if there is still one left
--- abort criterion
nextSymNode :: SymTree -> CSM (Maybe SymNode)
nextSymNode st = do
  opts <- getOpts
  uvn  <- getUVNodes
  case nextNode st of
    Nothing                        -> return Nothing
    Just n@(SymNode d cid _ _ _ _)
      | d > optSearchDepth opts    -> return Nothing
      | elemFM cid uvn             -> return (Just n)
      | otherwise                  -> nextSymNode (delNode d st)

--- Generate SMT-LIB commands for the variable declarations
--- and the assertion of path constraints for the given symbolic node
genSMTCmds :: SymNode -> CSM [Command]
genSMTCmds (SymNode _ cid vds pcs _ v) = do
  uvn <- getUVNodes
  case lookupFM uvn cid of
    Nothing                   -> error "Search.genSMTCmds"
    Just (CaseInfo _ ts nvds) -> return $ vds ++ nvds
                                              ++ [assert (v `noneOf` ts : pcs)]

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
  resetSMTStack
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

--- Communication with SMT solver

--- Initialize a solver session for concolic testing
initSession :: Solver -> SMTInfo -> IO SolverSession
initSession solver smtInfo = do
  s <- newSession solver
  return $ bufferCmds s (smtDecls smtInfo ++ initScopeCmds)

--- Reset the internal stack of the SMT solver
resetSMTStack :: CSM ()
resetSMTStack = modify $
  \s -> s { cssSession =  bufferCmds (cssSession s) [Pop 1, Push 1] }

--- Lift an SMT solver operation to CSM
csm :: SMTOp a -> CSM a
csm computation = do
  s              <- get
  (res, session) <- io $ computation (cssSession s)
  put s { cssSession = session }
  return res
