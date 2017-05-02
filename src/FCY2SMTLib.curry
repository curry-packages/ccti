--- ----------------------------------------------------------------------------
--- This module transforms FlatCurry type declarations into corresponding
--- SMTLib datatype declarations.
--- Furthermore, a bidirectional map is generated mapping FlatCurry constructors
--- to their SMTLib representation and vice versa.
---
--- @author  Jan Tikovsky
--- @version April 2017
--- ----------------------------------------------------------------------------
module FCY2SMTLib where

import Char            (toLower)
import FiniteMap
import FlatCurry.Types

import           Bimap
import           FlatCurryGoodies    (prel)
import qualified SMTLib.Types as SMT
import           Utils               ((<$>), mapM)

--- Bidirectional constructor map
--- mapping FlatCurry constructors to SMTLib constructors and vice versa
type ConsMap = BM QName SMT.Ident

--- Bidirectional type map
--- mapping FlatCurry types to SMTLib sorts and vice versa
type TypeMap = BM QName SMT.Sort

data SMTState = SMTState
  { smtDecls :: [SMT.Command]
  , smtTMap  :: TypeMap
  , smtCMap  :: ConsMap
  }

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
newSMTSort qn = modify $ \s -> let tmap = smtTMap s in case lookupBM qn tmap of
  Nothing -> s { smtTMap = addToBM qn (SMT.SComb (snd qn) []) tmap }
  Just _  -> error "FCY2SMTLib.newSMTSort"

--- Create an SMTLib constructor for the given FlatCurry constructor
newSMTCons :: QName -> SMTTrans ()
newSMTCons qn = modify $ \s -> let cmap = smtCMap s in case lookupBM qn cmap of
  Nothing -> s { smtCMap = addToBM qn (SMT.SComb (snd qn) []) cmap }
  Just _  -> error "FCY2SMTLib.newSMTCons"

fcy2SMT :: [TypeDecl] -> SMTState
fcy2SMT = snd . runSMTTrans . mapM tdecl2SMT

tdecl2SMT :: TypeDecl -> SMTTrans ()
tdecl2SMT (Type qn@(_, t) _ tvs cs) = do
  newSMTSort qn
  return (SMT.DeclareDatatypes (map show tvs)
    (lookupWithDefaultFM predefTypes t qn) <$> (mapM cdecl2SMT cs))
tdecl2SMT (TypeSyn         _ _ _ _) = return ()

cdecl2SMT :: ConsDecl -> SMTTrans SMT.ConsDecl
cdecl2SMT (Cons qn@(_, c) _ _ tys) = do
  newSMTCons qn
  return (SMT.Cons (lookupWithDefaultFM predefCons (map toLower c) qn)
    (mapM ty2SMT tys))

ty2SMT :: TypeExpr -> SMTTrans SMT.Sort
ty2SMT (TVar               v) = return $ SMT.SComb (show v) []
ty2SMT (FuncType     ty1 ty2) = return $ SMT.SComb "Func" <$> mapM ty2SMT [ty1, ty2]
ty2SMT (TCons qn@(_, tc) tys) = return $
  SMT.SComb (lookupWithDefaultFM predefTypes tc qn) <$> mapM ty2SMT tys

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

--- predefined SMTLib representations
--- predefined types
predefTypes :: FM QName SMT.Symbol
predefTypes = listToFM (<) $ map (\(x, y) -> (prel x, y))
  [ ("()","Unit"), ("[]","List"), ("(,)","Tuple2"), ("(,,)","Tuple3")
  , ("(,,,)","Tuple4"), ("(,,,,)","Tuple5")
  ]

--- predefined constructors
predefCons :: FM QName SMT.Ident
predefCons = listToFM (<) $ map (\(x, y) -> (prel x, y))
  [ ("()","unit"), ("[]","nil"), ("(,)","tuple2"), ("(,,)","tuple3")
  , ("(,,,)","tuple4"), ("(,,,,)","tuple5")
  ]
