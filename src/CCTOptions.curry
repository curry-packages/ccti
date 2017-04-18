--- --------------------------------------------------------------------------
--- Command line options for the concolic testing tool.
---
--- @author  Jan Tikovsky
--- @version April 2017
--- --------------------------------------------------------------------------
module CCTOptions
  (Options (..), Verbosity (..), badUsage, defOptions, getOpts) where

import FilePath (splitSearchPath)
import GetOpt   ( OptDescr (..), ArgOrder (Permute), ArgDescr (..), getOpt
                , usageInfo
                )
import IO       (hPutStrLn, stderr)
import List     (intercalate, maximum, nub)
import System   (exitWith, getArgs, getProgName)

import Utils    (rpad)

data Options = Options
  { optHelp        :: Bool
  , optVersion     :: Bool
  , optImportPaths :: [String]
  , optVerbosity   :: Verbosity
  }

--- default options
defOptions :: Options
defOptions = Options
  { optHelp        = False
  , optVersion     = False
  , optImportPaths = []
  , optVerbosity   = Status
  }

--- Verbosity level
data Verbosity = Quiet | Status | Info | Debug
  deriving (Eq, Ord)

--- Description and flag of verbosities
verbosities :: [(Verbosity, String, String)]
verbosities = [ (Quiet , "0", "quiet"       )
              , (Status, "1", "show status" )
              , (Info  , "2", "show symbolic trace")
              , (Debug , "3", "show information useful for debugging")
              ]

--- version information of ccti
version :: String
version = unlines [ "Curry Concolic Testing Interpreter"
                  , "Version 0.0.1"
                  ]

--- Description of the available options
options :: [OptDescr (OptErr Options -> OptErr Options)]
options =
  [ Option ['h', '?'] ["help"]
      (NoArg (onOpts $ \opts -> opts { optHelp = True }))
      "print usage information and exit"
  , Option ['V'] ["version"]
      (NoArg (onOpts $ \opts -> opts { optVersion = True }))
      "print version information and exit"
  , Option ['i'] ["import-path"]
      (ReqArg (onOptsArg $ \arg opts -> opts { optImportPaths = nub
        ((optImportPaths opts) ++ splitSearchPath arg) }) "dir[:dir]")
      "search for imports in `dir[:dir]'"
  , mkOption ['v'] ["verbosity"] verbDescriptions "n" "verbosity level"
  ]

--- Verbosity descriptions
verbDescriptions :: OptTable Options
verbDescriptions = map toDescr verbosities
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set f opts = opts { optVerbosity = f }

--- Retrieve the parsed options. This operation only returns if
---  * the @--help@ option was not specified
---  * the @--version@ option was not specified
---  * there were no errors in the specified options.
getOpts :: IO (Options, [String])
getOpts = do
  args <- getArgs
  prog <- getProgName
  processOpts prog $ parseOpts args

--- Parse the options specified on the command line.
parseOpts :: [String] -> (Options, [String], [String])
parseOpts args = (opts, files, errs ++ argErrs)
  where
    (opts, argErrs)        = foldl (flip ($)) (defOptions, []) optErrs
    (optErrs, files, errs) = getOpt Permute options args

--- Process the parsed options.
processOpts :: String -> (Options, [String], [String])
            -> IO (Options, [String])
processOpts prog (opts, files, errs)
  | optHelp    opts  = printUsage prog
  | optVersion opts  = printVersion
  | not (null errs') = badUsage prog errs'
  | otherwise        = return (opts, files)
  where errs' = errs ++ checkOpts opts files

--- Check the specified options for errors.
checkOpts :: Options -> [String] -> [String]
checkOpts _ []    = ["no files"]
checkOpts _ (_:_) = []

--- Print the usage information.
printUsage :: String -> IO a
printUsage prog = do
  putStrLn $ usageInfo header options
  exitWith 0
    where header = "usage: " ++ prog ++ " [OPTION] ... MODULE ..."

--- Complain about bad program usage (wrong options)
--- and print the usage information before exiting with error code 1.
badUsage :: String -> [String] -> IO a
badUsage prog errs = do
  mapIO_ (hPutStrLn stderr) errs
  hPutStrLn stderr $ "Try '" ++ prog ++ " --help' for more information"
  exitWith 1

--- Print the version string and exit
printVersion :: IO a
printVersion = do
  putStrLn version
  exitWith 0

-- -----------------------------------------------------------------------------

--- Type synonym for option and error message
type OptErr opts = (opts, [String])

--- An option table is a list of triples consisting of the option string,
--- its description and the effect on the global option set.
type OptTable opts = [(String, String, opts -> opts)]

--- Lift a function on options to options and errors.
onOpts :: (opts -> opts) -> OptErr opts -> OptErr opts
onOpts f (opts, errs) = (f opts, errs)

--- Lift a function on a string and options to options and errors.
onOptsArg :: (String -> opts -> opts) -> String -> OptErr opts -> OptErr opts
onOptsArg f arg (opts, errs) = (f arg opts, errs)

--- Add an error message to a option/errors pair.
addErr :: String -> OptErr opts -> OptErr opts
addErr err (opts, errs) = (opts, errs ++ [err])

--- Convert an option table to option descriptions capable of error handling.
mkOption :: [Char] -> [String] -> OptTable opts -> String -> String
       -> OptDescr (OptErr opts -> OptErr opts)
mkOption flags longFlags tbl arg what = Option flags longFlags
  (ReqArg (parseOptErr what tbl) arg)
  ("set " ++ what ++ " `" ++ arg ++ "', where `" ++ arg ++ "' is one of\n"
    ++ renderOptErrTable tbl)

--- Parsing function for option specified as an option table.
parseOptErr :: String -> OptTable opts -> String -> OptErr opts -> OptErr opts
parseOptErr what table opt = case lookup3 opt table of
  Just f  -> onOpts f
  Nothing -> addErr $ "unrecognized " ++ what ++ '`' : opt ++ "'\n"
 where
  lookup3 _ []                  = Nothing
  lookup3 k ((k', _, v2) : kvs)
    | k == k'                   = Just v2
    | otherwise                 = lookup3 k kvs

--- Rendering of an option specified using an option table.
renderOptErrTable :: OptTable opts -> String
renderOptErrTable ds = intercalate "\n"
                     $ map (\(k, d, _) -> "  " ++ rpad maxLen k ++ ": " ++ d) ds
  where maxLen = maximum $ map (\(k, _, _) -> length k) ds
