--- ----------------------------------------------------------------------------
--- This module defines some auxiliary functions for intermediate output.
---
--- @author  Jan Tikovsky
--- @version April 2017
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
status opts msg = when (optVerbosity opts >= Status) (putStrLn msg)

--- Print additional information
info :: CCTOpts -> String -> IO ()
info opts msg = when (optVerbosity opts >= Info) (putStrLn msg)

--- Print debug information
debug :: CCTOpts -> String -> IO ()
debug opts msg = when (optVerbosity opts >= Debug) (putStrLn msg)

--- Print debug information on concolic evaluation
debugEval :: CCTOpts -> String -> IO ()
debugEval opts msg
  = when (optDumpEval opts || optVerbosity opts >= Debug) (putStrLn msg)

--- Print debug information on concolic search
debugSearch :: CCTOpts -> String -> IO ()
debugSearch opts msg
  = when (optDumpSearch opts || optVerbosity opts >= Debug) (putStrLn msg)
