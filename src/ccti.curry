module ccti (main) where

import Directory                 (doesFileExist)
import Distribution              ( FrontendParams, FrontendTarget (FCY)
                                 , callFrontendWithParams
                                 , defaultParams, fullPath, getLoadPathForModule
                                 , inCurrySubdir, lookupModuleSourceInLoadPath
                                 , rcParams, setFullPath, setQuiet, setSpecials
                                 , stripCurrySuffix, sysLibPath
                                 )
import FileGoodies               (getFileInPath)
import FilePath                  ((<.>), (</>), takeFileName)
import FlatCurry.Annotated.Types
import List                      (nub)
import Maybe                     (fromMaybe)
import ReadShowTerm              (readUnqualifiedTerm)
import System                    (exitWith, getProgName)

import CCTOptions                (CCTOpts (..), badUsage, getOpts)
import FCY2SMTLib                (fcy2SMT)
import FlatCurryGoodies          (getMainBody, printExpr)
import IdentifyCases             (idCases)
import Output                    (info, status)
import PrettyPrint hiding        ((</>))
import Search                    (csearch)

-- TODO: remove when genSMTCmds was moved
-- import SMTLib.Types
-- import SMTLib.Goodies
-- import Symbolic (symInfo, SymInfo (..))
-- import FCY2SMTLib (declConst, SMTInfo (..))
-- import FiniteMap (foldFM)

main :: IO ()
main = do
  (opts, files) <- getOpts
  exec          <- getProgName
  params        <- rcParams
  -- TODO: adapt parameters when Distribution library was modified to accept
  -- annotated flatcurry as valid FrontendTarget
  let params' = setSpecials "--typed-flat"
              $ setFullPath (nub (sysLibPath ++ optImportPaths opts)) params
  status opts "Generating FlatCurry code"
  callFrontendWithParams FCY params' (head files)
  status opts "Reading FlatCurry file(s)"
  prog@(AProg m _ _ _ _) <- readAnnFlatCurry (head files)
  case getMainBody prog of
    Nothing -> badUsage exec ["There is no main function in the given Curry file"]
    Just  e -> do
      (ts, fs) <- getTysFuns m
      status opts "Annotating case expressions with fresh identifiers"
      let (fs', v) = idCases fs
      info opts (pPrint $ text "Functions:" <+> ppFuncDecls (filter (isLocal m) fs'))
      status opts "Generating SMT-LIB declarations for FlatCurry types"
      let smtInfo  = fcy2SMT ts
      info opts (pPrint $ text "Generated SMTLIB declarations:" <+> pretty smtInfo)
      status opts "Beginning with concolic search"
      testCases <- csearch opts fs' v smtInfo e
      putStr $ pPrint $ vsep $ map ppTestCase testCases

isLocal :: String -> AFuncDecl TypeExpr -> Bool
isLocal m (AFunc qn _ _ _ _) = m == fst qn

-- TODO: nicht transformierte Funktionen des Main Moduls werden verwendet,
-- Aufruf von getTysFuns anpassen

-- Get all local functions as well as all directly and indirectly imported ones
getTysFuns :: String -> IO ([TypeDecl], [AFuncDecl TypeExpr])
getTysFuns mod = do
  (ts, fs, _) <- getTysFuns' [] [] [] mod
  return (ts, fs)
 where
  getTysFuns' ts fs ms m
    | m `elem` ms = return (ts, fs, ms)
    | otherwise   = do
        (AProg _ is mts mfs _) <- readAnnFlatCurry m
        foldIO (\(ts', fs', ms') i -> getTysFuns' ts' fs' ms' i)
               (ts ++ mts, fs ++ mfs, m:ms)
               is

-- genSMTCmds :: CEState -> SMTLib
-- genSMTCmds state = SMTLib (tds ++ vds ++ pcs)
--   where
--     smtInfo = cesSMTInfo state
--     tds     = smtDecls smtInfo
--     vds     = foldFM (\vi ti cs -> declConst vi ti : cs) [] (smtVars smtInfo)
--     pcs     = genConstrs (map symInfo (cesTrace state))
--
-- genConstrs :: [SymInfo] -> [Command]
-- genConstrs [] = []
-- genConstrs ((SymInfo _ vi t):sis)
--   | null sis  = [Assert (tvar vi /=% t), CheckSat, GetModel]
--   | otherwise = Assert (tvar vi =% t) : genConstrs sis

-- TODO: Remove when there is support for reading annotated FlatCurry files
-- in the libraries
readAnnFlatCurry :: String -> IO (AProg TypeExpr)
readAnnFlatCurry progname = readAnnFlatCurryWithParseOptions progname
  (setSpecials "--typed-flat" (setQuiet True defaultParams))

readAnnFlatCurryWithParseOptions :: String -> FrontendParams -> IO (AProg TypeExpr)
readAnnFlatCurryWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find FlatCurry file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileInPath (annFlatCurryFileName (takeFileName progname)) [""]
                                loadpath
      readAnnFlatCurryFile filename
    Just (dir,_) -> do
      callFrontendWithParams FCY options progname
      readAnnFlatCurryFile (annFlatCurryFileName (dir </> takeFileName progname))

annFlatCurryFileName :: String -> String
annFlatCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "tfcy"

readAnnFlatCurryFile :: String -> IO (AProg TypeExpr)
readAnnFlatCurryFile filename = do
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
