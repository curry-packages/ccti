--- ----------------------------------------------------------------------------
--- This module provides operations for an interactive communication with
--- SMT solvers - which implement the SMT-LIB interface - via stdin and stdout.
--- Currently only the Z3 SMT solver is supported.
---
--- @author  Jan Tikovsky
--- @version October 2017
--- ----------------------------------------------------------------------------
module SMT.Solver where

import IO
import IOExts                        (execCmd)
import List                          (partition)
import Text.Pretty

import           Language.SMTLIB.Goodies      (isDeclData)
import           Language.SMTLIB.Parser       (parseCmdRsps)
import           Language.SMTLIB.Pretty
import qualified Language.SMTLIB.Types as SMT

data Solver = SMTSolver { executable :: String, flags :: [String] }

--- solver result
data Result = Error  [Message]
            | Unsat
            | Unknown
            | Sat
            | Model  [SMT.ModelRsp]
            | Values [SMT.ValuationPair]
  deriving Show

instance Pretty Result where
  pretty (Error msgs) = vsep $ map pretty msgs
  pretty Unsat        = text "unsat"
  pretty Unknown      = text "unknown"
  pretty Sat          = text "sat"
  pretty (Model   ms) = parent (map pretty ms)
  pretty (Values vps) = parent (map ppValPair vps)

--- error messages
data Message = SolverError String
             | ParserError String
             | OtherError  String
  deriving Show

--- pretty printing of error messages
instance Pretty Message where
  pretty (SolverError err) = text "Solver Error:" <+> text err
  pretty (ParserError err) = text "Parser Error:" <+> text err
  pretty (OtherError  err) = text "Error:"        <+> text err

--- smart constructor for parser errors
parserError :: String -> Result
parserError str = Error [ParserError str]

--- smart constructor for solver errors
errorMsgs :: [SMT.CmdResponse] -> Result
errorMsgs rsps = case rsps of
  [] -> Error [OtherError "No response"]
  _  -> Error $ map message rsps
 where
  message :: SMT.CmdResponse -> Message
  message rsp = case rsp of
    SMT.ErrorRsp   msg -> SolverError msg
    SMT.UnsupportedRsp -> SolverError "Unsupported command"
    _                  -> OtherError $ "Unexpected response: " ++ show rsp

--- z3 solver configuration
z3 :: Solver
z3 = SMTSolver { executable = "z3", flags = ["-smt2", "-in"] }

--- A solver session includes
---   * handles for communicating with the solver
---   * a buffer for SMT-LIB commands
---   * a trace of SMT-LIB commands (only required for debugging purposes)
data SolverSession = Session
  { handles :: (Handle, Handle, Handle)
  , buffer  :: [SMT.Command]
  , trace   :: [SMT.Command]
  }

--- Get the stdin handle of a solver session
stdin :: SolverSession -> Handle
stdin (Session (sin, _, _) _ _) = sin

stdin2 :: SMT Handle
stdin2 = gets ((\(x, _, _) -> x) . handles)

stdout2 :: SMT Handle
stdout2 = gets ((\(_, y, _) -> y) . handles)

--- Get the stdout handle of a solver session
stdout :: SolverSession -> Handle
stdout (Session (_, sout, _) _ _) = sout

--- Start a new SMT solver session
newSession :: Solver -> IO SolverSession
newSession solver = do
  hs <- execCmd $ unwords $ executable solver : flags solver
  return $ Session hs [] []

newSession2 :: Solver -> SMT SolverSession
newSession2 solver = do
  hs <- liftIOA $ execCmd $ unwords $ executable solver : flags solver
  return $ Session hs [] []

--- Terminate an SMT solver session
termSession :: SolverSession -> IO ()
termSession s = sendCmds [SMT.Exit] s >> hClose (stdin s)

termSession2 :: SMT ()
termSession2 = sendCmds2 [SMT.Exit] >> stdin2 >>= liftIOA . hClose

--- Buffer the given SMT-LIB commands
bufferCmds :: SolverSession -> [SMT.Command] -> SolverSession
bufferCmds s cmds = s { buffer = (buffer s) ++ cmds }

bufferCmds2 :: [SMT.Command] -> SMT ()
bufferCmds2 cmds = modify $ \s -> s { buffer = buffer s ++ cmds }

--- SMT solver operation
type SMTOp a = SolverSession -> IO (a, SolverSession)

--------------------------------------------------------------------------------

--- SMT monad (state transformer with IO)
data SMT a = SMT { runSMT :: SolverSession -> IO (a, SolverSession) }

instance Monad SMT where
  return x = SMT $ \s -> return (x, s)

  (SMT m) >>= f = SMT $ \s -> do
    (r, s') <- m s
    runSMT (f r) s'

gets :: (SolverSession -> a) -> SMT a
gets f = SMT $ \s -> return (f s, s)

get :: SMT SolverSession
get = gets id

getBuffer :: SMT [SMT.Command]
getBuffer = gets buffer

getTrace :: SMT [SMT.Command]
getTrace = gets trace

put :: SolverSession -> SMT ()
put s = SMT $ \_ -> return ((), s)

modify :: (SolverSession -> SolverSession) -> SMT ()
modify f = SMT $ \s -> return ((), f s)

--- Lift an IO action to SMT
liftIOA :: IO a -> SMT a
liftIOA m = SMT $ \s -> m >>= (\x -> return (x, s))

--------------------------------------------------------------------------------

--- Check for syntactic errors as well as for satisfiability of the assertions
checkSat :: SMTOp Result
checkSat s = do
  errMsg <- getDelimited s
  -- check for syntactic errors, type mismatches etc.
  case parseCmdRsps errMsg of
    Left  msg                -> return (parserError msg, s)
    Right rs | not (null rs) -> return (errorMsgs rs   , s)
             | otherwise     -> do
      (_,s') <- sendCmds [SMT.CheckSat] s
      satMsg <- getDelimited s'
      -- check satisfiability
      case parseCmdRsps satMsg of
        Left  msg                           -> return (parserError msg, s')
        Right [SMT.CheckSatRsp SMT.Unknown] -> return (Unknown        , s')
        Right [SMT.CheckSatRsp SMT.Unsat]   -> return (Unsat          , s')
        Right [SMT.CheckSatRsp SMT.Sat]     -> return (Sat            , s')
        Right rsps                          -> return (errorMsgs rsps , s')

--- Check for syntactic errors as well as for satisfiability of the assertions
checkSat2 :: SMT Result
checkSat2 = do
  errMsg <- getDelimited2
  -- check for syntactic errors, type mismatches etc.
  case parseCmdRsps errMsg of
    Left  msg                -> return $ parserError msg
    Right rs | not (null rs) -> return $ errorMsgs rs
             | otherwise     -> do
      sendCmds2 [SMT.CheckSat]
      satMsg <- getDelimited2
      -- check satisfiability
      case parseCmdRsps satMsg of
        Left  msg                           -> return $ parserError msg
        Right [SMT.CheckSatRsp SMT.Unknown] -> return $ Unknown
        Right [SMT.CheckSatRsp SMT.Unsat]   -> return $ Unsat
        Right [SMT.CheckSatRsp SMT.Sat]     -> return $ Sat
        Right rsps                          -> return $ errorMsgs rsps


--- Get a model for the current assertions on the solver stack
getModel :: SMTOp Result
getModel s = do
  (_,s')   <- sendCmds [SMT.GetModel] s
  modelMsg <- getDelimited s'
  case parseCmdRsps modelMsg of
    Left  msg                 -> return (parserError msg, s')
    Right [SMT.GetModelRsp m] -> return (Model m        , s')
    Right rsps                -> return (errorMsgs rsps , s')

--- Get a model for the current assertions on the solver stack
getModel2 :: SMT Result
getModel2 = do
  sendCmds2 [SMT.GetModel]
  modelMsg <- getDelimited2
  case parseCmdRsps modelMsg of
    Left  msg                 -> return $ parserError msg
    Right [SMT.GetModelRsp m] -> return $ Model m
    Right rsps                -> return $ errorMsgs rsps

--- Get a binding for the given variables considering the current assertions
--- on the solver stack
getValues :: [SMT.Term] -> SMTOp Result
getValues ts s = do
  (_,s') <- sendCmds [SMT.GetValue ts] s
  valMsg <- getDelimited s'
  case parseCmdRsps valMsg of
    Left  msg                 -> return (parserError msg, s')
    Right [SMT.GetValueRsp m] -> return (Values m       , s')
    Right rsps                -> return (errorMsgs rsps , s')

--- Get a binding for the given variables considering the current assertions
--- on the solver stack
getValues2 :: [SMT.Term] -> SMT Result
getValues2 ts = do
  sendCmds2 [SMT.GetValue ts]
  valMsg <- getDelimited2
  case parseCmdRsps valMsg of
    Left  msg                 -> return $ parserError msg
    Right [SMT.GetValueRsp m] -> return $ Values m
    Right rsps                -> return $ errorMsgs rsps

--- Add delimiter to stdout via echo command in order to read answers successively
--- and send commands to solver
sendCmds :: [SMT.Command] -> SMTOp ()
sendCmds cmds s = do
  let buffered = buffer s ++ cmds ++ [SMT.Echo delim]
      sin      = stdin s
  hPutStr sin $ showSMT buffered
  hFlush  sin
  return ((), s { buffer = [], trace = (trace s) ++ buffered })

--- Add delimiter to stdout via echo command in order to read answers successively
--- and send commands to solver
sendCmds2 :: [SMT.Command] -> SMT ()
sendCmds2 cmds = do
  bufferCmds2 (cmds ++ [SMT.Echo delim])
  sin <- stdin2
  buf <- getBuffer
  liftIOA $ hPutStr sin (showSMT buf) >> hFlush sin
  modify $ \s -> s { buffer = [], trace = trace s ++ buf }

--- Get the contents of stdout of a solver session up to the delimiter "END-OF-ANSWER"
getDelimited :: SolverSession -> IO String
getDelimited s = hGetUntil (stdout s) delim

getDelimited2 :: SMT String
getDelimited2 = stdout2 >>= \h -> liftIOA $ hGetUntil h delim

--- ----------------------------------------------------------------------------
--- Special operations for concolic testing
--- ----------------------------------------------------------------------------

--- Initialize a solver session for concolic testing
initSession :: Solver -> [SMT.Command] -> IO SolverSession
initSession solver cmds = do
  s <- newSession solver
  return $ bufferCmds s (cmds ++ [SMT.Push 1])

--- Initialize a solver session for concolic testing
initSession2 :: Solver -> [SMT.Command] -> SMT ()
initSession2 solver cmds = do
  newSession2 solver
  bufferCmds2 (cmds ++ [SMT.Push 1])

--- Restart SMT solver session for concolic testing
restartSession :: Solver -> SMTOp ()
restartSession solver s = do
  let t        = trace s
      (ds, cs) = partition isDeclData t
  termSession s
  s' <- newSession solver
  return ((), s' { trace = cs, buffer = ds })

--- Restart SMT solver session for concolic testing
restartSession2 :: Solver -> SMT ()
restartSession2 solver = do
  t <- getTrace
  let (ds, cs) = partition isDeclData t
  termSession2
  newSession2 solver
  modify $ \s -> s { trace = cs, buffer = ds }

--- Reset the internal stack of the SMT solver (for incremental solving)
resetStack :: SolverSession -> IO ((), SolverSession) -- SMTOP ()
resetStack s = return ((), bufferCmds s [SMT.Pop 1, SMT.Push 1])

--- Reset the internal stack of the SMT solver (for incremental solving)
resetStack2 :: SMT ()
resetStack2 = bufferCmds2 [SMT.Pop 1, SMT.Push 1]

-- helper

--- delimiter for solver answers
delim :: String
delim = "END-OF-ANSWER"

--- Reads the contents from an input handle up to the given delimiter
--- and leaves the handle open
hGetUntil :: Handle -> String -> IO String
hGetUntil h d = do
  l <- hGetLine h
  if l == d then return ""
            else hGetUntil h d >>= \ls -> return (l ++ '\n' : ls)
