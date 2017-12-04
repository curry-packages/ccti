--- ----------------------------------------------------------------------------
--- This module defines some auxiliary functions for intermediate output.
---
--- @author  Jan Tikovsky
--- @version December 2017
--- ----------------------------------------------------------------------------
module Output where

import Unsafe (unsafePerformIO)

import CCTOptions (CCTOpts (..), Verbosity (..))

traceEval :: CCTOpts -> String -> a -> a
traceEval opts msg x = unsafePerformIO (debugEval opts msg >> return x)

traceInfo :: CCTOpts -> String -> a -> a
traceInfo opts msg x = unsafePerformIO (info opts msg >> return x)

traceStatus :: CCTOpts -> String -> a -> a
traceStatus opts msg x = unsafePerformIO (status opts msg >> return x)

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
  = when (optDebugEval opts || optVerbosity opts >= Debug) (putMsg msg)

--- Print debug information on concolic search
debugSearch :: CCTOpts -> String -> IO ()
debugSearch opts msg
  = when (optDebugSearch opts || optVerbosity opts >= Debug) (putMsg msg)

--- Write the given String to the command line with a preceeding line of `=`
putMsg :: String -> IO ()
putMsg msg = putStrLn (replicate 120 '=') >> putStrLn msg
