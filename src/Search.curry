module Search where

import FiniteMap       (FM, addToFM, emptyFM, lookupFM)
import FlatCurry.Types
import List            (delete)

import Symbolic        ( BranchNr, Decision (..), NthOfM (..), PathConstr (..)
                       , SymInfo (..), SymTree (..), Trace, addTrace
                       )
import Utils           (mapM_)

--- mapping of constructor variables to their argument variables
type ArgMap = FM VarIndex [VarIndex]

--- map of unvisited case branches
type BranchMap = FM VarIndex [Int]

--- function call with arguments and the corresponding result for a test case
type TestCase = (Expr, Expr)

data CSState = CSState
  { cssCCTOpts :: CCTOpts
  , cssTree    :: SymTree
  , cssUnvisBs :: BranchMap
  , cssArgs    :: ArgMap
  , cssTests   :: [TestCase]
  }

initCSState :: CCTOpts -> CSState
initCSState opts  = CSState
  { cssCCTOpts = opts
  , cssTree    = Leaf
  , cssUnvisBs = emptyFM (<)
  , cssArgs    = emptyFM (<)
  , cssTests   = []
  }

--------------------------------------------------------------------------------

--- Concolic search monad
data CSM a = CS { runCSM :: CSState -> (a, CSState) }

instance Monad CSM where
  return x = CS $ \s -> (x, s)

  (CS f) >>= g = CS $ \s -> let (x, s') = f s in (runCSM (g x)) s'

gets :: (CSState -> a) -> CSM a
gets f = CS $ \s -> (f s, s)

get :: CSM CSState
get = gets id

put :: CSState -> CSM ()
put s = CS $ \_ -> ((), s)

modify :: (CSState -> CSState) -> CSM ()
modify f = CS $ \s -> ((), f s)

--------------------------------------------------------------------------------

--- Get current symbolic tree
getSymTree :: CSM SymTree
getSymTree = gets cssTree

--- Merge the given trace with the symbolic tree in the current state
mergeTrace :: Trace -> CSM ()
mergeTrace tr = modify $ \s -> s { cssTree = addTrace tr (cssTree s) }

--------------------------------------------------------------------------------

--- Get current map of unvisited branches
getBranchMap :: CSM BranchMap
getBranchMap = gets cssUnvisBs

--- Mark the given branch as visited
visitBranch :: VarIndex -> BranchNr -> CSM ()
visitBranch cid (n :/: m) = do
  bsMap <- getBranchMap
  let bs' = case lookupFM bsMap cid of
              Nothing -> [b | b <- [1 .. m], b /= n]
              Just bs -> delete m bs
  modify $ \s -> s { cssUnvisBs = addToFM bsMap cid bs' }

--- Check if the given case expression has unvisited branches
hasUnvisited :: VarIndex -> CSM Bool
hasUnvisited cid = do
  bsMap <- getBranchMap
  case lookupFM bsMap cid of
    Nothing -> error $ "Search.hasUnvisited: Could not find case expression " ++ show cid
    Just bs -> return $ not $ null bs

--- Check if all branches were visited
allBsVisited :: CSM Bool
allBsVisited = do
  bsMap <- getBranchMap
  return $ all null $ eltsFM bsMap

--------------------------------------------------------------------------------

--- Get current argument map
getArgMap :: CSM ArgMap
getArgMap = gets cssArgs

--- Add the arguments for a symbolic constructor to the current argument map
addArgs :: VarIndex -> [VarIndex] -> CSM ()
addArgs v args
  | null args = return ()
  | otherwise = modify $ \s -> s { cssArgs = addToFM (cssArgs s) v args }

--------------------------------------------------------------------------------

--- Get current test cases
getTestCases :: CSM [TestCase]
getTestCases = gets cssTests

--- Add a test case to the current test cases
addTestCase :: TestCase -> CSM ()
addTestCase tc = modify $ \s -> s { cssTests = tc : cssTests s }

--------------------------------------------------------------------------------

updCSState :: Trace -> TestCase -> CSM ()
updCSState tr tc = do
  addTestCase tc
  mapM_ processDec tr
  mergeTrace tr

--- Process a given symbolic decision:
---   * add arguments of symbolic constructors to argument map and
---   * mark branches as visited
processDec :: Decision -> CSM ()
processDec (cid :>: (SymInfo bnr v _ args)) = do
  addArgs v args
  visitBranch cid bnr

getPathCs :: CSM [PathConstr]
getPathCs = do
  tree  <- getSymTree
  bsMap <- getBranchMap
  case tree of
    Leaf        -> return []
    Node cid bs -> do
      done <- hasUnvisited cid
      if done then 

--------------------------------------------------------------------------------

csearch :: Expr -> CSM ()
csearch e = do
  (res, tr) <- ceval opts eenv fs v e
  updCSState tr (e, res)
  done <- allBsVisited
  unless done $ do
    pcs <- getPathCs
