--- ----------------------------------------------------------------------------
--- This module provides operations for an interactive communication with
--- SMT solvers - which implement the SMT-LIB interface - via stdin and stdout.
--- Currently only the Z3 SMT solver is supported.
---
--- @author  Jan Tikovsky
--- @version June 2017
--- ----------------------------------------------------------------------------
module SMTLib.Solver where

import IO
import IOExts                        (execCmd)
import List                          (partition)

import           PrettyPrint
import           SMTLib.Goodies      (isDeclData)
import           SMTLib.Parser       (parseCmdRsps)
import           SMTLib.Pretty
import qualified SMTLib.Types as SMT

data Solver = SMT { executable :: String, flags :: [String] }

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
z3 = SMT { executable = "z3", flags = ["-smt2", "-in"] }

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

--- Get the stdout handle of a solver session
stdout :: SolverSession -> Handle
stdout (Session (_, sout, _) _ _) = sout

--- Start a new SMT solver session
newSession :: Solver -> IO SolverSession
newSession solver = do
  hs <- execCmd $ unwords $ executable solver : flags solver
  return $ Session hs [] []

--- Terminate an SMT solver session
termSession :: SolverSession -> IO ()
termSession s = sendCmds [SMT.Exit] s >> hClose (stdin s)

--- Buffer the given SMT-LIB commands
bufferCmds :: SolverSession -> [SMT.Command] -> SolverSession
bufferCmds s cmds = s { buffer = (buffer s) ++ cmds }

--- SMT solver operation
type SMTOp a = SolverSession -> IO (a, SolverSession)

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

--- Get a model for the current assertions on the solver stack
getModel :: SMTOp Result
getModel s = do
  (_,s')   <- sendCmds [SMT.GetModel] s
  modelMsg <- getDelimited s'
  case parseCmdRsps modelMsg of
    Left  msg                 -> return (parserError msg, s')
    Right [SMT.GetModelRsp m] -> return (Model m        , s')
    Right rsps                -> return (errorMsgs rsps , s')

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

--- Add delimiter to stdout via echo command in order to read answers successively
--- and send commands to solver
sendCmds :: [SMT.Command] -> SMTOp ()
sendCmds cmds s = do
  let buffered = buffer s ++ cmds ++ [SMT.Echo delim]
      sin      = stdin s
  hPutStr sin $ showSMT buffered
  hFlush  sin
  return ((), s { buffer = [], trace = (trace s) ++ buffered })

--- Get the contents of stdout of a solver session up to the delimiter "END-OF-ANSWER"
getDelimited :: SolverSession -> IO String
getDelimited s = hGetUntil (stdout s) delim

--- ----------------------------------------------------------------------------
--- Special operations for concolic testing
--- ----------------------------------------------------------------------------

--- Initialize a solver session for concolic testing
initSession :: Solver -> [SMT.Command] -> IO SolverSession
initSession solver cmds = do
  s <- newSession solver
  return $ bufferCmds s (cmds ++ [SMT.Push 1])

--- Restart SMT solver session for concolic testing
restartSession :: Solver -> SMTOp ()
restartSession solver s = do
  let t        = trace s
      (ds, cs) = partition isDeclData t
  termSession s
  s' <- newSession solver
  return ((), s' { trace = cs, buffer = ds })

--- Reset the internal stack of the SMT solver (for incremental solving)
resetStack :: SolverSession -> IO ((), SolverSession) -- SMTOP ()
resetStack s = return ((), bufferCmds s [SMT.Pop 1, SMT.Push 1])

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

-- test

-- example :: IO ()
-- example = do
--   s <- newSession z3
--   addCmds s [ SMT.DeclareDatatypes ["a"] "Maybe" [SMT.Cons "nothing" [], SMT.Cons "just" [SMT.SV "just_1" (SMT.SComb "a" [])]]
--             , SMT.DeclareDatatypes [] "Unit" [SMT.Cons "unit" []]
--             ]
--   addCmds s [ SMT.DeclareConst "x1" (SMT.SComb "Maybe" [SMT.SComb "Bool" []])
--             , SMT.DeclareConst "x2" (SMT.SComb "Maybe" [SMT.SComb "Bool" []])
--             ]
--   addCmds s [ SMT.Assert (SMT.TComb (SMT.Id "not") [SMT.TComb (SMT.Id "=") [SMT.TComb (SMT.Id "x2") [], SMT.TComb (SMT.Id "nothing") []]]) -- (SMT.As "nothing" (SMT.SComb "Maybe" [SMT.SComb "Unit" []])) []]])
--             , SMT.Assert (SMT.TComb (SMT.Id "=") [SMT.TComb (SMT.Id "x2") [], SMT.TComb (SMT.Id "x1") []])
--             ]
--   sat <- checkSat s
--   case sat of
--     Sat -> do
--       t1 <- getValues s [SMT.TComb (SMT.Id "x1") []]
--       putStrLn (show t1)
--       checkSat s
--       t2 <- getValues s [SMT.TComb (SMT.Id "x2") []]
--       putStrLn (show t2)
--       terminateSession s
--     _   -> terminateSession s

