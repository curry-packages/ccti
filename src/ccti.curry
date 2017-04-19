module ccti (main) where

import Distribution     ( FrontendTarget (FCY), callFrontendWithParams, fullPath
                        , rcParams, setFullPath, sysLibPath
                        )
import FlatCurry.Files  (readFlatCurry)
import FlatCurry.Types
import List             (nub)
import Maybe            (fromMaybe)
import System           (exitWith, getProgName)

import CCTOptions       (CCTOpts (..), badUsage, getOpts)
import EnumEnv          (enumerate)
import Eval             (ceval)
import FlatCurryGoodies (fcall, hasMain, printExpr)
import IdentifyCases    (idCases, ICState (..))
import Output           (status)

main :: IO ()
main = do
  (opts, files) <- getOpts
  exec          <- getProgName
  params        <- rcParams
  let params' = setFullPath (nub (sysLibPath ++ optImportPaths opts)) params
  status opts "Generating FlatCurry code"
  callFrontendWithParams FCY params' (head files)
  status opts "Reading FlatCurry file(s)"
  prog@(Prog m _ _ _ _) <- readFlatCurry (head files)
  if not (hasMain prog)
    then badUsage exec ["There is no main function in the given Curry file"]
    else do (ts, fs) <- getAllFuncs m
            status opts "Evaluating normal form of main"
            printExpr $ ceval opts (enumerate ts) (idCases fs) (fcall (m, "main") [])

-- TODO: nicht transformierte Funktionen des Main Moduls werden verwendet,
-- Aufruf von getAllFuncs anpassen

-- Get all local functions as well as all directly and indirectly imported ones
getAllFuncs :: String -> IO ([TypeDecl], [FuncDecl])
getAllFuncs mod = getAllFuncs' [] [] [] mod >>= \(types, funs, _) -> return (types, funs)
  where
    getAllFuncs' ts fs ms m
      | m `elem` ms = return (ts, fs, ms)
      | otherwise   = do
          (Prog _ is mts mfs _) <- readFlatCurry m
          foldIO (\(ts', fs', ms') i -> getAllFuncs' ts' fs' ms' i) (ts ++ mts, fs ++ mfs, m:ms) is
