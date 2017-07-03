--- ----------------------------------------------------------------------------
--- This module defines some auxiliary functions for intermediate output.
---
--- @author  Jan Tikovsky
--- @version June 2017
--- ----------------------------------------------------------------------------
module Output where

import Unsafe (unsafePerformIO)

import CCTOptions (CCTOpts (..), Verbosity (..))

traceEval :: CCTOpts -> String -> a -> a
traceEval opts msg x = unsafePerformIO (debugEval opts msg >> return x)

traceInfo :: CCTOpts -> String -> a -> a
traceInfo opts msg x = unsafePerformIO (info opts msg >> return x)

--- Print status information
status :: CCTOpts -> String -> IO ()
status opts msg = when (optVerbosity opts >= Status) (putMsg msg)

--- Print additional information
info :: CCTOpts -> String -> IO ()
info opts msg = when (optVerbosity opts >= Info) (putMsg msg)

--- Print debug information
debug :: CCTOpts -> String -> IO ()
debug opts msg = when (optVerbosity opts >= Debug) (putMsg msg)

--- Print debug information on concolic evaluation
debugEval :: CCTOpts -> String -> IO ()
debugEval opts msg
  = when (optDumpEval opts || optVerbosity opts >= Debug) (putMsg msg)

--- Print debug information on concolic search
debugSearch :: CCTOpts -> String -> IO ()
debugSearch opts msg
  = when (optDumpSearch opts || optVerbosity opts >= Debug) (putMsg msg)

--- Write a dump of the SMT-LIB commands which were generated during search
--- to a file
dumpSMT :: CCTOpts -> String -> IO ()
dumpSMT opts smtScript
  = when (optDumpSMT opts) (writeFile "smtDump.smt" smtScript)

--- Write the given String to the command line with a preceeding line of `=`
putMsg :: String -> IO ()
putMsg msg = putStrLn (replicate 120 '=') >> putStrLn msg
