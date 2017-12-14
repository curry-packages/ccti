--- ----------------------------------------------------------------------------
--- This module provides operations for reading TypedFlatCurry files.
---
--- @author  Jan Tikovsky
--- @version December 2017
--- ----------------------------------------------------------------------------
module ReadTFCY (readTFCY) where

import Directory    (doesFileExist)
import Distribution ( FrontendParams, FrontendTarget (TFCY), addTarget
                    , callFrontendWithParams, defaultParams
                    , getLoadPathForModule, inCurrySubdir
                    , lookupModuleSourceInLoadPath, setQuiet, stripCurrySuffix
                    )
import FileGoodies  (getFileInPath)
import FilePath     ((<.>), (</>), takeFileName)
import ReadShowTerm (readUnqualifiedTerm)


--- Parse a Curry program and return corresponding TypedFlatCurry program
readTFCY :: String -> IO (AProg TypeExpr)
readTFCY progname = readTFCYWithParseOptions progname
  (addTarget TFCY (setQuiet True defaultParams))

--- Parse a Curry program with parsing options and return corresponding
--- TypedFlatCurry program
readTFCYWithParseOptions :: String -> FrontendParams -> IO (AProg TypeExpr)
readTFCYWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find FlatCurry file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileInPath (tfcyFile (takeFileName progname)) [""]
                                loadpath
      readTFCYFile filename
    Just (dir,_) -> do
      callFrontendWithParams TFCY options progname
      readTFCYFile (tfcyFile (dir </> takeFileName progname))

--- Transform name of a Curry program into file name of corresponding
--- TypedFlatCurry program
tfcyFile :: String -> String
tfcyFile prog = inCurrySubdir (stripCurrySuffix prog) <.> "tfcy"

--- Reads a TypedFlatCurry program from a file in ".tfcy" format
readTFCYFile :: String -> IO (AProg TypeExpr)
readTFCYFile filename = do
  exafcy <- doesFileExist filename
  if exafcy
   then readExistingAFCY filename
   else do let subdirfilename = inCurrySubdir filename
           exdirfcy <- doesFileExist subdirfilename
           if exdirfcy
            then readExistingAFCY subdirfilename
            else error ("EXISTENCE ERROR: FlatCurry file '" ++ filename ++
                        "' does not exist")
 where
   readExistingAFCY fname = do
     filecontents <- readFile fname
     return (readUnqualifiedTerm ["FlatCurry.Types","FlatCurry.Annotated.Types","Prelude"] filecontents)
