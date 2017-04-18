--- ----------------------------------------------------------------------------
--- This module defines some auxiliary functions for intermediate output.
---
--- @author  Jan Tikovsky
--- @version April 2017
--- ----------------------------------------------------------------------------
module Output where

import Unsafe (unsafePerformIO)

import CCTOptions (Options (..), Verbosity (..))

traceDebug :: Options -> String -> a -> a
traceDebug opts msg x = unsafePerformIO (debug opts msg >> return x)

--- Print status information
status :: Options -> String -> IO ()
status opts msg = when (optVerbosity opts >= Status) (putStrLn msg)

--- Print additional information
info :: Options -> String -> IO ()
info opts msg = when (optVerbosity opts >= Info) (putStrLn msg)

--- Print debug information
debug :: Options -> String -> IO ()
debug opts msg = when (optVerbosity opts >= Debug) (putStrLn msg)
