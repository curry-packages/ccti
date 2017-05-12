--- ----------------------------------------------------------------------------
--- This module transforms FlatCurry type declarations into corresponding
--- SMTLib datatype declarations.
--- Furthermore, bidirectional maps are generated mapping FlatCurry types
--- as well as FlatCurry constructors to their SMTLib representation and vice
--- versa.
---
--- @author  Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module FCY2SMTLib where

import Char            (toLower)
import FiniteMap
import FlatCurry.Types
import List            (isPrefixOf)

import           Bimap
import           FlatCurryGoodies    (prel)
import           PrettyPrint
import           SMTLib.Pretty
import qualified SMTLib.Types as SMT
import           Utils               ((<$>), mapM)

--- Bidirectional constructor map
--- mapping FlatCurry constructors to SMTLib constructors and vice versa
type ConsMap = BM QName SMT.Ident

--- Bidirectional type map
--- mapping FlatCurry types to SMTLib sorts and vice versa
type TypeMap = BM QName SMT.Ident

data SMTState = SMTState
  { smtDecls :: [SMT.Command]
  , smtTMap  :: TypeMap
  , smtCMap  :: ConsMap
  }

initSMTState :: SMTState
initSMTState = SMTState
  { smtDecls = []
  , smtTMap  = predefTypes
  , smtCMap  = predefCons
  }

--- Pretty printing of an SMTState
instance Pretty SMTState where
  pretty (SMTState ds tmap cmap) = vsepBlank
    [ text "Generated SMTLib datatype declarations"
    , vsep (map pretty ds)
    , text "Type Map: Mapping FlatCurry types to SMT sorts"
    , ppBM ppEntry tmap
    , text "Constructor Map: Mapping FlatCurry constructors to SMT constructors"
    , ppBM ppEntry cmap
    ]
    where ppEntry (k, v) = ppQName k <+> rarrow <+> text v

--- SMT transformation monad
data SMTTrans a = SMTTrans { runSMTTrans :: SMTState -> (a, SMTState) }

instance Monad SMTTrans where
  return x = SMTTrans $ \s -> (x, s)

  (SMTTrans f) >>= g = SMTTrans $ \s -> let (x, s') = f s
                                        in  (runSMTTrans (g x)) s'

modify :: (SMTState -> SMTState) -> SMTTrans ()
modify f = SMTTrans $ \s -> ((), f s)

--- Add an SMT datatype declaration
addSMTDecl :: SMT.Command -> SMTTrans ()
addSMTDecl d = modify (\s -> s { smtDecls = d : smtDecls s })

--- Create an SMTLib sort representation for the given FlatCurry type
newSMTSort :: QName -> SMTTrans ()
newSMTSort qn@(_, tc) = modify $ \s -> let tmap = smtTMap s
  in case lookupBM qn tmap of
       Nothing -> s { smtTMap = addToBM qn tc tmap }
       Just _  -> s

--- Create an SMTLib constructor for the given FlatCurry constructor
newSMTCons :: QName -> SMTTrans ()
newSMTCons qn@(_, c) = modify $ \s -> let cmap = smtCMap s
  in case lookupBM qn cmap of
       Nothing -> s { smtCMap = addToBM qn (map toLower c) cmap }
       Just _  -> s

fcy2SMT :: [TypeDecl] -> SMTState
fcy2SMT ts = snd $ (runSMTTrans $ mapM tdecl2SMT ts) initSMTState

tdecl2SMT :: TypeDecl -> SMTTrans ()
tdecl2SMT td = case td of
  Type qn@(_, t) _ tvs cs
    | not $ any (`isPrefixOf` t) ignoredTypes -> do
        newSMTSort qn
        cs' <- mapM cdecl2SMT cs
        addSMTDecl (SMT.DeclareDatatypes (map (typeVars !!) tvs)
          (lookupWithDefaultBM t qn predefTypes) cs')
  _                                        -> return ()

cdecl2SMT :: ConsDecl -> SMTTrans SMT.ConsDecl
cdecl2SMT (Cons qn@(_, c) _ _ tys) = do
  newSMTCons qn
  tys' <- mapM ty2SMT tys
  let c' = map toLower (lookupWithDefaultBM c qn predefCons)
  return $ SMT.Cons c' (zipWith SMT.SV (map (\n -> c' ++ '_' : show n) [1..]) tys')

ty2SMT :: TypeExpr -> SMTTrans SMT.Sort
ty2SMT (TVar               v) = return $ SMT.SComb (typeVars !! v) []
ty2SMT (FuncType     ty1 ty2) = SMT.SComb "Func" <$> mapM ty2SMT [ty1, ty2]
ty2SMT (TCons qn@(_, tc) tys) =
  SMT.SComb (lookupWithDefaultBM tc qn predefTypes) <$> mapM ty2SMT tys

-- map predefined types and constructors to the corresponding SMTLib name

-- im trace werden in jeder decision, der case branch der varindex,
-- der smtlib konstruktor sowie die argumentvariablen des entspr. konstruktors
-- gespeichert

-- für jede decision muss später in SMT eine variable deklariert werden
-- hierzu müssen zusätzlich typinformationen gespeichert werden
-- speichere die smtlib representation des jeweiligen typkonstruktors in decision mit ab?

-- eventuell ist es einfacher annotated flatcurry zu verwenden und dann bei der
-- konkolischen interpretation einfach den annotierten typ eines case branches
-- in die entsprechende smtlib darstellung zu transformieren?

--- infinite list of type variables
typeVars :: [String]
typeVars = [c : if n == 0 then [] else show n |  n <- [0 ..], c <- ['a' .. 'z']]

--- predefined SMTLib representations

--- predefined basic types and type constructors
predefTypes :: BM QName SMT.Ident
predefTypes = listToBM (<) (<) $ map qualPrel
  [ ("Bool","Bool"), ("Int","Int"), ("Float","Float"), ("[]","List")
  , ("()","Unit"), ("[]","List"), ("(,)","Tuple2"), ("(,,)","Tuple3")
  , ("(,,,)","Tuple4"), ("(,,,,)","Tuple5"), ("(,,,,,)","Tuple6")
  , ("(,,,,,,)","Tuple7"), ("(,,,,,,,)","Tuple8"), ("(,,,,,,,,)","Tuple9")
  , ("(,,,,,,,,,)","Tuple10"), ("(,,,,,,,,,,)","Tuple11")
  , ("(,,,,,,,,,,,)","Tuple12"), ("(,,,,,,,,,,,,)","Tuple13")
  , ("(,,,,,,,,,,,,,)","Tuple14"), ("(,,,,,,,,,,,,,,)","Tuple15")
  ]

--- predefined constructors
predefCons :: BM QName SMT.Ident
predefCons = listToBM (<) (<) $ map qualPrel
  [ ("False","false"), ("True","true"), ("[]","nil"), (":","insert")
  , ("()","unit"), ("[]","nil"), (":", "insert"), ("(,)","tuple2"), ("(,,)","tuple3")
  , ("(,,,)","tuple4"), ("(,,,,)","tuple5"), ("(,,,,,)","tuple6")
  , ("(,,,,,,)","tuple7"), ("(,,,,,,,)","tuple8"), ("(,,,,,,,,)","tuple9")
  , ("(,,,,,,,,,)","tuple10"), ("(,,,,,,,,,,)","tuple11")
  , ("(,,,,,,,,,,,)","tuple12"), ("(,,,,,,,,,,,,)","tuple13")
  , ("(,,,,,,,,,,,,,)","tuple14"), ("(,,,,,,,,,,,,,,)","tuple15")
  ]

--- data types which are ignored regarding the generation of SMT data type declarations
ignoredTypes :: [String]
ignoredTypes =  ["Bool", "Int", "Float", "Char", "_Dict", "IO", "[]", "(->)"]

-- helper
-- qualify first component of a tuple with "Prelude"
qualPrel :: (String, b) -> (QName, b)
qualPrel (x, y) = (prel x, y)
