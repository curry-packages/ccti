module ccti (main) where

import Distribution               ( sysLibPath )
import FilePath                   (takeDirectory)
import FlatCurry.Annotated.Pretty (ppFuncDecls)
import FlatCurry.Annotated.Types
import List                       (nub)
import System                     (getProgName)
import Text.Pretty

import CCTOptions          (CCTOpts (..), badUsage, getOpts)
import FCYFunctorInstances
import FCY2SMTLib          (fcy2SMT)
import FlatCurryGoodies    (extendAnn, getMainBody)
import IdentifyCases       (idCases)
import Output              (debug, status)
import ReadTFCY
import Search              (csearch, ppTestCase)
import Utils               (inDirectory)

import System.FrontendExec ( FrontendParams, FrontendTarget (TFCY)
                           , callFrontendWithParams, rcParams, setFullPath )

main :: IO ()
main = do
  (opts, files) <- getOpts
  exec          <- getProgName
  params        <- rcParams
  let params' = setFullPath (nub (sysLibPath ++ optImportPaths opts)) params
  status opts "Generating FlatCurry code"
  let modpath = head files
  callFrontendWithParams TFCY params' modpath
  status opts "Reading FlatCurry file(s)"
  prog@(AProg m _ _ _ _) <- readTFCYFile $ tfcyFile modpath
  case getMainBody prog of
    Nothing -> badUsage exec ["The module must include a main function with a function call in its body."]
    Just  e -> do
      (ts, fs) <- inDirectory (takeDirectory modpath) (getTysFuns m)
      status opts "Annotating case expressions with fresh identifiers"
      let (fs', v) = idCases fs
          e'       = fmap extendAnn e
      debug opts (pPrint $ text "Annotated Functions:" <+> ppFuncDecls  fs')
      status opts "Generating SMT-LIB declarations for FlatCurry types"
      let smtInfo  = fcy2SMT ts
      debug opts (pPrint $ text "Generated SMTLIB declarations:" <+> pretty smtInfo)
      status opts "Beginning with concolic search"
      testCases <- csearch opts fs' v smtInfo e'
      status opts "Printing generated test cases:"
      putStrLn $ pPrint $ vsep $ map ppTestCase testCases

-- Get all local functions as well as all directly and indirectly imported ones
getTysFuns :: String -> IO ([TypeDecl], [AFuncDecl TypeExpr])
getTysFuns mod = do
  (ts, fs, _) <- getTysFuns' [] [] [] mod
  return (ts, fs)
 where
  getTysFuns' ts fs ms m
    | m `elem` ms = return (ts, fs, ms)
    | otherwise   = do
        (AProg _ is mts mfs _) <- readTFCY m
        foldIO (\(ts', fs', ms') i -> getTysFuns' ts' fs' ms' i)
               (ts ++ mts, fs ++ mfs, m:ms)
               is
