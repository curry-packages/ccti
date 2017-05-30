module Search where

import FiniteMap                 ( FM, addListToFM_C, addToFM, delFromFM, elemFM
                                 , emptyFM, lookupFM )
import FlatCurry.Annotated.Types
import List                      (delete, union)

import CCTOptions                (CCTOpts (..))
import FCY2SMTLib                (SMTInfo (..), toSort, toTerm, fromTerm)
import Eval                      (ceval)
import FlatCurryGoodies          (getTypes, replArgs)
import Output
import PrettyPrint
import Search.DFS
import SMTLib.Goodies            ((=%), assert, noneOf, tvar, var2SMT)
import SMTLib.Solver
import SMTLib.Types              (Command (..), Sort, Term)
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

--- Type environment storing the associated FlatCurry type as well as the
--- SMT-LIB sort for SMT variables
type TypeEnv = FM VarIndex TypeInfo

data TypeInfo = TypeInfo { fcType :: TypeExpr, smtSort :: Sort }

--- Generate a new TypeInfo object
newTypeInfo :: SMTInfo -> TypeExpr -> TypeInfo
newTypeInfo smtInfo ty = TypeInfo ty (toSort smtInfo ty)

--- Generate a variable declaration in SMT-LIB
declConst :: VarIndex -> TypeInfo -> Command
declConst vi (TypeInfo _ s) = DeclareConst (var2SMT vi) s

--- Test case data: function call with arguments and corresponding result
type TestCase = (AExpr TypeExpr, AExpr TypeExpr)

data CSState = CSState
  { cssCCTOpts :: CCTOpts
  , cssSMTInfo :: SMTInfo
  , cssTree    :: SymTree
  , cssUVNodes :: UVNodes
  , cssTypeEnv :: TypeEnv
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
  , cssTypeEnv = emptyFM (<)
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

--- Merge the given trace with the symbolic tree in the current state
-- mergeTrace :: Trace -> CSM ()
-- mergeTrace tr = modify $ \s -> s { cssTree = addTrace tr (cssTree s) }

--------------------------------------------------------------------------------


--- Mark the given branch as visited
-- visitBranch :: VarIndex -> BranchNr -> CSM ()
-- visitBranch cid (n :/: m) = do
--   bsMap <- getUVNodes
--   let bs' = case lookupFM bsMap cid of
--               Nothing -> [b | b <- [1 .. m], b /= n]
--               Just bs -> delete m bs
--   modify $ \s -> s { cssUVNodes = addToFM bsMap cid bs' }

--- Check if the given case expression has unvisited branches
-- hasUnvisited :: VarIndex -> CSM Bool
-- hasUnvisited cid = do
--   bsMap <- getUVNodes
--   case lookupFM bsMap cid of
--     Nothing -> error $ "Search.hasUnvisited: Could not find case expression " ++ show cid
--     Just bs -> return $ not $ null bs

--- Check if all branches were visited
-- allBsVisited :: CSM Bool
-- allBsVisited = do
--   bsMap <- getUVNodes
--   return $ all null $ eltsFM bsMap

--------------------------------------------------------------------------------

--- Get current test cases
-- getTestCases :: CSM [TestCase]
-- getTestCases = gets cssTests

--- Add a test case to the current test cases
addTestCase :: TestCase -> CSM ()
addTestCase tc = modify $ \s -> s { cssTests = tc : cssTests s }

--------------------------------------------------------------------------------

type UpdSymInfo = SymTree -> UVNodes -> TypeEnv -> (SymTree, UVNodes, TypeEnv)

updSymInfo :: Trace -> CSM ()
updSymInfo t = modify $ \s ->
  let (st, uvn, te) = processTrace t (cssSMTInfo s) (cssTree s) (cssUVNodes s)
        (cssTypeEnv s)
  in s { cssTree = st, cssUVNodes = uvn, cssTypeEnv = te }

processTrace :: Trace -> SMTInfo -> UpdSymInfo
processTrace trace smtInfo tree uvNodes tyEnv
  = prcTrace trace 0 [] [] tree uvNodes tyEnv
  where
  prcTrace []                                        _ _   _  st uvn te = (st, uvn, te)
  prcTrace (Decision cid bnr v c@(_, cty) args : ds) d vds cs st uvn te =
        -- SMT-LIB term representation of selected FlatCurry constructor
    let t     = toTerm smtInfo c args
        -- type information for SMT variables
        vtys  = zip (args ++ [v]) (map (newTypeInfo smtInfo) $ getTypes cty)
        -- additional SMT-LIB variable declarations required for this node
        vdecs = map (uncurry declConst) vtys
        -- extended SMT-LIB variable declarations
        vds'  = vds ++ vdecs
        -- extended list of path constraints
        cs'   = cs ++ [tvar v =% t]
        -- extended symbolic tree
        st'   = addNode (SymNode d cid vds cs v) st
        -- updated unvisited nodes
        uvn'  = visitBranch cid bnr t vdecs uvn
        -- updated type environment
        te'   = addListToFM_C specialize te vtys
    in prcTrace ds (d+1) vds' cs' st' uvn' te'
  -- specialize type information
  specialize old new = case old of
    (TypeInfo (TVar _) _) -> new
    _                     -> old

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
  -- initialize solver session
  status opts "Initializing solver session"
  session <- newSession z3
  addCmds session (smtDecls smtInfo ++ [Push 1])
  (_, s) <- runCSM (searchLoop fs v e) (initCSState opts smtInfo session)
  -- terminate solver session
  terminateSession session
  return (cssTests s)

--- main loop of concolic search
searchLoop :: [AFuncDecl TypeExpr] -> VarIndex -> AExpr TypeExpr -> CSM ()
searchLoop fs v e = do
  opts <- getOpts
  let (res, ts, args, v') = ceval opts fs v e
  io $ status opts $ "Found test case: " ++ pPrint (ppExp e <+> equals <+> ppExp res)
  addTestCase (e, res)
  mapM_ updSymInfo ts
  st  <- getSymTree
  nxt <- nextSymNode st
  case nxt of
    Nothing -> return ()
    Just  n -> do
      io $ status opts $ "Next node: " ++ show n
      cmds    <- genSMTCmds n
      session <- getSession
      es      <- solve session (map tvar args) cmds
      if null es then return ()
                 else searchLoop fs v' (replArgs e es)

-- TODO: Overthink abstractions for symbolic tree
-- this method should be provided by the interface of symbolic tree
--- Get next symbolic node from search tree if there is still one left
--- abort criterion
nextSymNode :: SymTree -> CSM (Maybe SymNode)
nextSymNode st = do
  opts <- getOpts
  uvn  <- getUVNodes
  case nextNode st of
    Nothing                     -> return Nothing
    Just n@(SymNode d cid _ _ _)
      | d > optSearchDepth opts -> return Nothing
      | elemFM cid uvn          -> return (Just n)
      | otherwise               -> nextSymNode (delNode d st)

--- Generate SMT-LIB commands for the variable declarations
--- and the assertion of path constraints for the given symbolic node
genSMTCmds :: SymNode -> CSM [Command]
genSMTCmds (SymNode _ cid vds pcs v) = do
  uvn <- getUVNodes
  case lookupFM uvn cid of
    Nothing                   -> error "Search.genSMTCmds"
    Just (CaseInfo _ ts nvds) -> return $ vds ++ nvds
                                              ++ [assert (v `noneOf` ts : pcs)]

--- Run an SMT solver to compute FlatCurry arguments for given SMT variables
solve :: SolverSession -> [Term] -> [Command] -> CSM [AExpr TypeExpr]
solve s ts cmds = do
  opts    <- getOpts
  smtInfo <- getSMTInfo
  io $ do
    -- remove constraints from previous iteration
    resetStack s
    -- add constraints to solver stack
    addCmds s cmds
    -- check satisfiability
    isSat <- checkSat s
    debug opts $ "Check satisfiability: " ++ pPrint (pretty isSat)
    case isSat of
      Sat -> do
        vals <- getValues s ts
        debug opts $ "Get values: " ++ pPrint (pretty vals)
        case vals of
          Values vps -> return $ map (fromTerm smtInfo . snd) vps
          r          -> debug opts (pPrint (pretty r)) >> return []
      r   -> debug opts (pPrint (pretty r)) >> return []

