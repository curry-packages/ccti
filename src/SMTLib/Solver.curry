--- ----------------------------------------------------------------------------
--- This module provides operations for an interactive communication with
--- SMT solvers - which implement the SMT-LIB interface - via stdin and stdout.
--- Currently only the Z3 SMT solver is supported.
---
--- @author  Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module SMTLib.Solver where

import IO
import IOExts                        (execCmd)

import           PrettyPrint
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
  pretty (Model    _) = text "model"
  pretty (Values   _) = text "valuation pair"

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

type SolverSession = (Handle, Handle, Handle)

--- Start a new SMT solver session
newSession :: Solver -> IO SolverSession
newSession solver = execCmd $ unwords $ executable solver : flags solver

--- Terminate an SMT solver session
terminateSession :: SolverSession -> IO ()
terminateSession s@(sin, _, _) = sendCmds s [SMT.Exit] >> hClose sin

--- Add given SMTLib commands to SMT solver
addCmds :: SolverSession -> [SMT.Command] -> IO ()
addCmds (sin, _, _) cmds = hPutStr sin $ pPrint $ pretty $ SMT.SMTLib cmds

--- Enter a new scope
enterScope :: SolverSession -> IO ()
enterScope = flip addCmds [SMT.Push 1]

--- Leave the current scope
exitScope :: SolverSession -> IO ()
exitScope = flip addCmds [SMT.Pop 1]

--- Reset internal stack of SMT solver
resetStack :: SolverSession -> IO ()
resetStack = flip addCmds [SMT.Pop 1, SMT.Push 1]

--- Check for syntactic errors as well as for satisfiability of the assertions
checkSat :: SolverSession -> IO Result
checkSat s = do
  sendCmds s []
  errMsg <- getDelimited s
  -- check for syntactic errors, type mismatches etc.
  case parseCmdRsps errMsg of
    Left  msg  -> return $ parserError msg
    Right rs | not (null rs) -> return $ errorMsgs rs
             | otherwise     -> do
      sendCmds s [SMT.CheckSat]
      satMsg <- getDelimited s
      -- check satisfiability
      case parseCmdRsps satMsg of
        Left  msg                           -> return $ parserError msg
        Right [SMT.CheckSatRsp SMT.Unknown] -> return Unknown
        Right [SMT.CheckSatRsp SMT.Unsat]   -> return Unsat
        Right [SMT.CheckSatRsp SMT.Sat]     -> return Sat
        Right rsps                          -> return $ errorMsgs rsps

--- Get a model for the current assertions on the solver stack
getModel :: SolverSession -> IO Result
getModel s = do
  sendCmds s [SMT.GetModel]
  modelMsg <- getDelimited s
  case parseCmdRsps modelMsg of
    Left  msg                 -> return $ parserError msg
    Right [SMT.GetModelRsp m] -> return $ Model m
    Right rsps                -> return $ errorMsgs rsps

--- Get a binding for the given variables considering the current assertions
--- on the solver stack
getValues :: SolverSession -> [SMT.Term] -> IO Result
getValues s ts = do
  sendCmds s [SMT.GetValue ts]
  valMsg <- getDelimited s
  case parseCmdRsps valMsg of
    Left  msg                 -> return $ parserError msg
    Right [SMT.GetValueRsp m] -> return $ Values m
    Right rsps                -> return $ errorMsgs rsps


--- Check for syntactic errors, if the model is satisfiable and compute model
-- checkNSolve :: SolverSession -> IO Result
-- checkNSolve s = do
--   sendCmds s []
--   errMsg <- getDelimited s
--   -- check for syntactic errors, type mismatches etc.
--   case parseCmdRsps errMsg of
--     Left  msg  -> return $ parserError msg
--     Right rs | not (null rs) -> return $ errorMsgs rs
--              | otherwise     -> do
--       sendCmds s [SMT.CheckSat]
--       satMsg <- getDelimited s
--       -- check satisfiability
--       case parseCmdRsps satMsg of
--         Left msg -> return $ parserError msg
--         Right [SMT.CheckSatRsp SMT.Unknown] -> return Unknown
--         Right [SMT.CheckSatRsp SMT.Unsat]   -> return Unsat
--         Right [SMT.CheckSatRsp SMT.Sat]     -> do
--           sendCmds s [SMT.GetModel]
--           modelMsg <- getDelimited s
--           -- compute model
--           case parseCmdRsps modelMsg of
--             Left  msg                 -> return $ parserError msg
--             Right [SMT.GetModelRsp m] -> return $ Sat m
--             Right rsps                -> return $ errorMsgs rsps
--         Right rsps -> return $ errorMsgs rsps

--- Add delimiter to stdout via echo command in order to read answers successively
--- and send commands to solver
sendCmds :: SolverSession -> [SMT.Command] -> IO ()
sendCmds s@(sin, _, _) cmds = addCmds s (cmds ++ [SMT.Echo delim]) >> hFlush sin

--- Get the contents of stdout of a solver session up to the delimiter "END-OF-ANSWER"
getDelimited :: SolverSession -> IO String
getDelimited (_, sout, _) = hGetUntil sout delim

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

