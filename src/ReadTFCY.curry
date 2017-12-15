--- ----------------------------------------------------------------------------
--- This module provides operations for reading TypedFlatCurry files.
---
--- @author  Jan Tikovsky
--- @version December 2017
--- ----------------------------------------------------------------------------
module ReadTFCY where

import Directory    (doesFileExist)
import Distribution ( FrontendParams, FrontendTarget (TFCY), addTarget
                    , callFrontendWithParams, defaultParams
                    , getLoadPathForModule, inCurrySubdir
                    , lookupModuleSourceInLoadPath, setQuiet, stripCurrySuffix
                    )
import FileGoodies  (getFileInPath)
import FilePath     ((<.>), (</>), takeFileName)
import ReadShowTerm (readUnqualifiedTerm)

import FlatCurry.Annotated.Types

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
readTFCYFile fn = do
  exafcy <- doesFileExist fn
  if exafcy
   then readExistingTFCY fn
   else do let subdirfilename = inCurrySubdir fn
           exdirfcy <- doesFileExist subdirfilename
           if exdirfcy
            then readExistingTFCY subdirfilename
            else error ("EXISTENCE ERROR: TypedFlatCurry file '" ++ fn ++
                        "' does not exist")
 where
 readExistingTFCY fname = do
   filecontents <- readFile fname
   return $ readUnqualifiedTerm
     ["FlatCurry.Types","FlatCurry.Annotated.Types","Prelude"] filecontents
