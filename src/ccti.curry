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
import EnumEnv          (enumerate, ppEEnv)
import Eval             (ceval)
import FlatCurryGoodies (fcall, hasMain, printExpr)
import IdentifyCases    (idCases)
import Output           (info, status)
import PrettyPrint

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
    else do
      (ts, fs) <- getTysFuns m
      status opts "Evaluating normal form of main"
      let eenv     = enumerate ts
          (fs', v) = idCases fs
      info opts (pPrint $ text "Enumeration environment:" <+> ppEEnv eenv)
      info opts (pPrint $ text "Functions:" <+> ppFuncDecls defaultOptions (filter (isLocal m) fs'))
      printExpr $ ceval opts eenv fs' v (fcall (m, "main") [])

isLocal :: String -> FuncDecl -> Bool
isLocal m (Func qn _ _ _ _) = m == fst qn

-- TODO: nicht transformierte Funktionen des Main Moduls werden verwendet,
-- Aufruf von getTysFuns anpassen

-- Get all local functions as well as all directly and indirectly imported ones
getTysFuns :: String -> IO ([TypeDecl], [FuncDecl])
getTysFuns mod = getTysFuns' [] [] [] mod >>= \(types, funs, _) -> return (types, funs)
  where
    getTysFuns' ts fs ms m
      | m `elem` ms = return (ts, fs, ms)
      | otherwise   = do
          (Prog _ is mts mfs _) <- readFlatCurry m
          foldIO (\(ts', fs', ms') i -> getTysFuns' ts' fs' ms' i)
                 (ts ++ mts, fs ++ mfs, m:ms)
                 is
