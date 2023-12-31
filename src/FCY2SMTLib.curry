--- ----------------------------------------------------------------------------
--- This module transforms FlatCurry type declarations into corresponding
--- SMTLib datatype declarations.
--- Furthermore, bidirectional maps are generated mapping FlatCurry types
--- as well as FlatCurry constructors to their SMTLib representation and vice
--- versa.
---
--- @author  Jan Tikovsky
--- @version January 2018
--- ----------------------------------------------------------------------------
module FCY2SMTLib where

import Char                                   (toLower)
import Data.FiniteMap
import FlatCurry.Annotated.Goodies            (argTypes, resultType)
import FlatCurry.Annotated.Pretty             (ppQName, ppTypeExp, ppVarIndex)
import FlatCurry.Annotated.Types
import List                                   (isPrefixOf)
import Text.Pretty hiding                     (compose)

import           Bimap
import           FCYFunctorInstances
import           FlatCurryGoodies
import           Language.SMTLIB.Goodies
import           Language.SMTLIB.Pretty
import qualified Language.SMTLIB.Types as SMT
import           Substitution
import           Symbolic                     ( CoveredCs (..), LConstr (..)
                                              , PathConstr, SymObj (..)
                                              , prelSymCons)
import           Utils                        ((<$>), mapFst, ppFM)

--- Bidirectional constructor map
--- mapping FlatCurry constructors to SMTLib constructors and vice versa
type ConsMap = BM SymObj SMT.Ident

--- Bidirectional type map
--- mapping FlatCurry types to SMTLib sorts and vice versa
type TypeMap = BM QName SMT.Ident

--- Type environment storing type information for SMT-LIB variables
type TypeEnv = FM VarIndex TypeInfo

data TypeInfo = TypeInfo
  { fcType  :: TypeExpr
  , smtSort :: SMT.Sort
  , args    :: [VarIndex]
  }
 deriving Show

instance Pretty TypeInfo where
  pretty (TypeInfo ty s args) =
    braces (ppTypeExp ty <+> pretty s <+> list (map ppVarIndex args))

--- Update the type environment
--- possibly specializing type information for existing entries
updTypeEnv :: [VarIndex] -> TypeExpr -> [VarIndex] -> SMTTrans ()
updTypeEnv vs cty args = do
  cty' <- rnmTVars cty
  sub  <- foldM mbAdd emptySubst $ zip3 vs (resArgTypes cty') (args : repeat [])
  modify $ \s -> s { smtVars = mapFM (specTypes s sub) (smtVars s) }
 where
  mbAdd sub (v, ty, as) = do
    s <- get
    if hasTypeInfo v s
      then do
        let sub' = sub `compose` (unify [(ty, getFCYType v s)])
            ty'  = subst sub' ty
        put s { smtVars = updFM (smtVars s) v (\_ -> typeInfo s ty' as) }
        return $ sub'
      else do
        put s { smtVars = addToFM (smtVars s) v (typeInfo s ty as) }
        return sub

--- Check, if there is type information for the given variable
hasTypeInfo :: VarIndex -> SMTInfo -> Bool
hasTypeInfo v smtInfo = elemFM v (smtVars smtInfo)

--- Specialize type information applying the given type substitution
specTypes :: SMTInfo -> TypeSubst -> VarIndex -> TypeInfo -> TypeInfo
specTypes smtInfo sub _ ti@(TypeInfo ty _ args)
  | ty == ty' = ti
  | otherwise = typeInfo smtInfo ty' args
  where ty' = subst sub ty

--- Generate a new TypeInfo object
typeInfo :: SMTInfo -> TypeExpr -> [VarIndex] -> TypeInfo
typeInfo smtInfo ty args = TypeInfo ty (toSort smtInfo ty) args

--- Rename all type variables in the given type expression
rnmTVars :: TypeExpr -> SMTTrans TypeExpr
rnmTVars ty = do
  let is = getTyVars ty
  tes <- map TVar <$> freshTVars (length is)
  return (subst (mkSubst is tes) ty)

--- Get n fresh type variable indices
freshTVars :: Int -> SMTTrans [TVarIndex]
freshTVars n = do
  s <- get
  let v = smtTVar s
  put s { smtTVar = v + n }
  return [v .. n-1]

--- Generate a single SMT-LIB constant declaration
declConst :: VarIndex -> TypeInfo -> SMT.Command
declConst vi (TypeInfo _ s _) = SMT.DeclareConst (var2SMT vi) s

--- Generate SMT-LIB constant declarations for the given indices
declConsts :: SMTInfo -> [VarIndex] -> [SMT.Command]
declConsts smtInfo vs = map (uncurry declConst) $ fmToList
                      $ filterFM (\v _ -> v `elem` vs) (smtVars smtInfo)

--- Generate SMT-LIB assertions for the given path constraints
assertConstr :: SMTInfo -> [PathConstr] -> CoveredCs -> VarIndex -> VarIndex
             -> SMT.Command
assertConstr smtInfo pcs cc dv vi = assert $ map (pc2SMT smtInfo) pcs ++ npc
  where
  -- negated path constraint
  npc = case cc of
    CConstr lc l -> [tnot $ pc2SMT smtInfo (dv, SymLit lc l, [])]
    CCons     cs -> snd $ foldr diffThan (vi, []) cs
      where
      diffThan :: (SymObj, [VarIndex]) -> (VarIndex, [SMT.Term]) -> (VarIndex, [SMT.Term])
      diffThan (sobj, args) (i, ineqs) =
        let ss = map (getSMTSort smtInfo) args
            vn = i - length ss
            vs = [i, i - 1 .. vn + 1]
        in (vn, forAll vs ss (tvar dv /=% qtcomb (cons2SMT smtInfo sobj dv)
                                                 (map tvar vs)) : ineqs)

--- Transform path constraints to SMT-LIB constraints
pc2SMT :: SMTInfo -> PathConstr -> SMT.Term
pc2SMT smtInfo pc = case pc of
  (v, tqn@(SymCons _ _), args) -> tvar v =% qtcomb (cons2SMT smtInfo tqn v)
                                              (map tvar args)
  (v, SymLit lc l      ,    _) -> (lc2SMT lc) (tvar v) (lit2SMT l)

data SMTInfo = SMTInfo
  { smtDecls :: [SMT.Command]
  , smtVars  :: TypeEnv
  , smtTMap  :: TypeMap
  , smtCMap  :: ConsMap
  , smtTVar  :: TVarIndex
  }

initSMTInfo :: SMTInfo
initSMTInfo = SMTInfo
  { smtDecls = []
  , smtVars  = emptyFM (<)
  , smtTMap  = predefTypes
  , smtCMap  = predefCons
  , smtTVar  = 0
  }

--- Pretty printing of an SMTInfo
instance Pretty SMTInfo where
  pretty (SMTInfo ds vmap tmap cmap tidx) = vsepBlank
    [ text "Generated SMTLib datatype declarations"
    , vsep (map pretty ds)
    , text "Var Map: Mapping SMT variables to FlatCurry types and SMTLib sorts"
    , ppFM (\(k, v) -> ppVarIndex k <+> text "\x27f6" <+> pretty v) vmap
    , text "Type Map: Mapping FlatCurry types to SMT sorts"
    , ppBM (\(k, v) -> ppQName k <+> text "\x27f7" <+> text v) tmap
    , text "Constructor Map: Mapping FlatCurry constructors to SMT constructors"
    , ppBM (\(k, v) -> pretty k <+>  text "\x27f7" <+> text v) cmap
    , int tidx
    ]

--- SMT transformation monad
data SMTTrans a = SMTTrans { runSMTTrans :: SMTInfo -> (a, SMTInfo) }

instance Monad SMTTrans where
  return x = SMTTrans $ \s -> (x, s)

  f >>= g = SMTTrans $ \s -> let (x, s') = runSMTTrans f s
                             in  (runSMTTrans (g x)) s'

get :: SMTTrans SMTInfo
get = SMTTrans $ \s -> (s, s)

put :: SMTInfo -> SMTTrans ()
put s = SMTTrans $ \_ -> ((), s)

modify :: (SMTInfo -> SMTInfo) -> SMTTrans ()
modify f = SMTTrans $ \s -> ((), f s)

--- Execute an SMT transformation
execSMTTrans :: SMTTrans a -> SMTInfo -> SMTInfo
execSMTTrans act smtInfo = snd $ runSMTTrans act smtInfo

--- Add an SMT datatype declaration
addSMTDecl :: SMT.Command -> SMTTrans ()
addSMTDecl d = modify (\s -> s { smtDecls = d : smtDecls s })

--- Lookup the SMT constructor for the given FlatCurry constructor
lookupSMTCons :: SymObj -> SMTInfo -> Maybe SMT.Ident
lookupSMTCons tqn smtInfo = lookupBM tqn (smtCMap smtInfo)

--- Lookup the FlatCurry constructor for the given SMT constructor
lookupFCYCons :: SMT.Ident -> SMTInfo -> Maybe SymObj
lookupFCYCons i smtInfo = lookupBMR i (smtCMap smtInfo)

--- Lookup the SMT type for the given FlatCurry type
lookupType :: QName -> SMTInfo -> Maybe SMT.Ident
lookupType qn smtInfo = lookupBM qn (smtTMap smtInfo)

--- Get FlatCurry type information for the given SMT-LIB variable
getFCYType :: VarIndex -> SMTInfo -> TypeExpr
getFCYType vi smtInfo = case lookupFM (smtVars smtInfo) vi of
  Nothing -> error $ "FCY2SMT.getFCYType: unbound variable " ++ show vi
  Just ti -> fcType ti

--- Get SMT-LIB sort information for the given SMT-LIB variable
getSMTSort :: SMTInfo -> VarIndex -> SMT.Sort
getSMTSort smtInfo vi = case lookupFM (smtVars smtInfo) vi of
  Nothing -> error $ "FCY2SMT.getSMTSort: unbound variable " ++ show vi
  Just ti -> smtSort ti

--- Get the arguments for the given SMT-LIB variable
getArgs :: VarIndex -> SMTInfo -> [VarIndex]
getArgs vi smtInfo = case lookupFM (smtVars smtInfo) vi of
  Nothing -> []
  Just ti -> args ti

--- Lookup or generate SMTLib sort representation for the given FlatCurry type
newSMTSort :: QName -> SMTTrans SMT.Symbol
newSMTSort qn@(_, tc) = do
  smtInfo <- get
  case lookupType qn smtInfo of
    Nothing  -> do put smtInfo { smtTMap = addToBM qn tc (smtTMap smtInfo) }
                   return tc
    Just tc' -> return tc'

--- Create an SMTLib constructor for the given typed FlatCurry constructor
newSMTCons :: SymObj -> SMTTrans ()
newSMTCons tqn@(SymCons qn _) = modify $
  \smtInfo -> case lookupSMTCons tqn smtInfo of
    Nothing ->
      smtInfo { smtCMap = addToBM tqn (map toLower (snd qn)) (smtCMap smtInfo) }
    Just _  -> smtInfo
newSMTCons (SymLit       _ _) = return ()

fcy2SMT :: [TypeDecl] -> SMTInfo
fcy2SMT ts = execSMTTrans (mapM tdecl2SMT ts) initSMTInfo

tdecl2SMT :: TypeDecl -> SMTTrans ()
tdecl2SMT td = case td of
  Type qn@(_, t) _ tvs cs
    | not $ any (`isPrefixOf` t) ignoredTypes -> do
        s   <- newSMTSort qn
        cs' <- mapM (cdecl2SMT (TCons qn (map TVar tvs))) cs
        addSMTDecl $
          SMT.DeclareDatatypes [(SMT.SortDecl s (length tvs), newDT tvs cs')]
  _                                           -> return ()

newDT :: [TVarIndex] -> [SMT.ConsDecl] -> SMT.DTDecl
newDT tvars cs
  | null tvars = SMT.MT cs
  | otherwise  = SMT.PT (map (typeVars !!) tvars) cs

cdecl2SMT :: TypeExpr -> ConsDecl -> SMTTrans SMT.ConsDecl
cdecl2SMT rty (Cons qn@(_, c) _ _ tys) = do
  let tqn = SymCons qn (mkFunType tys rty)
  newSMTCons tqn
  tys' <- mapM ty2SMT tys
  let c' = lookupWithDefaultBM (map toLower c) tqn predefCons
  return $ SMT.Cons c' (zipWith SMT.SV (map (\n -> c' ++ '_' : show n) [1..]) tys')

ty2SMT :: TypeExpr -> SMTTrans SMT.Sort
ty2SMT (ForallType       _ _) = error "FCY2SMT.ty2SMT: ForallType"
ty2SMT (TVar               v) = return $ SMT.SComb (typeVars !! v) []
ty2SMT (FuncType     ty1 ty2) = SMT.SComb "Func" <$> mapM ty2SMT [ty1, ty2]
ty2SMT (TCons qn@(_, tc) tys) =
  SMT.SComb (lookupWithDefaultBM tc qn predefTypes) <$> mapM ty2SMT tys

--- Transform a FlatCurry constructor to an SMT-LIB identifier
cons2SMT :: SMTInfo -> SymObj -> VarIndex -> SMT.QIdent
cons2SMT smtInfo tqn v = case lookupSMTCons tqn smtInfo of
  Nothing -> error $ "FCY2SMTLib.cons2SMT: No SMT-LIB representation for "
               ++ show tqn
  Just c  -> SMT.As c (getSMTSort smtInfo v)

--- Transform a literal constraint to SMT-LIB
lc2SMT :: LConstr -> SMT.Term -> SMT.Term -> SMT.Term
lc2SMT E  = (=%)
lc2SMT NE = (/=%)
lc2SMT L  = (<%)
lc2SMT LE = (<=%)
lc2SMT G  = (>%)
lc2SMT GE = (>=%)

--- Transform a FlatCurry literal to SMT-LIB
lit2SMT :: Literal -> SMT.Term
lit2SMT (Intc   i) = tint   i
lit2SMT (Floatc f) = tfloat f
lit2SMT (Charc  _) = error "SMTLib.Goodies.lit2SMT: Characters are not supported"

--- Transform an SMT-LIB term into a FlatCurry expression
fromTerm :: SMTInfo -> VarIndex -> SMT.Term -> AExpr TypeAnn
fromTerm smtInfo vi t = fmap extendAnn $ fromTerm' (getFCYType vi smtInfo) t
 where
  fromTerm' rty t' = case t' of
    SMT.TComb qi ts
      | i == "tvar" -> AComb unitType ConsCall (prel "()", unitType) []
      | otherwise   -> case lookupFCYCons i smtInfo of
        Just (SymCons qn ty) ->
          let ty' = subst (unify [(resultType ty, rty)]) ty
          in AComb rty ConsCall (qn, ty') (zipWith fromTerm' (argTypes ty') ts)
        _           -> error $ "FCY2SMTLib.fromTerm: No FCY representation for "
          ++ show qi
      where i = unqual qi
    SMT.TConst sc -> fromSpecConst sc
    _ -> error $ "FCY2SMTLib.fromTerm: " ++ show t'

--- Transform an SMT-LIB spec constant into a FlatCurry expression
fromSpecConst :: SMT.SpecConstant -> AExpr TypeExpr
fromSpecConst (SMT.Num i) = ALit intType   (Intc i)
fromSpecConst (SMT.Dec f) = ALit floatType (Floatc f)
fromSpecConst (SMT.Str _) = error "FCY2SMTLib.fromSpecConst: strings not supported yet"

--- Transform a FlatCurry type expression into an SMTLib sort
toSort :: SMTInfo -> TypeExpr -> SMT.Sort
toSort _       (ForallType _ _) = error "FCY2SMT.toSort"
toSort _       (TVar         _) = orderingSort
toSort smtInfo (FuncType t1 t2) = funSC (map (toSort smtInfo) [t1, t2])
toSort smtInfo (TCons   qn tys) = case lookupType qn smtInfo of
  Nothing -> error $ "Eval.toSort: No SMTLIB representation for type constructor "
                 ++ show qn
  Just tc -> scomb tc (map (toSort smtInfo) tys)

--- infinite list of type variables
typeVars :: [String]
typeVars = [c : if n == 0 then [] else show n |  n <- [0 ..], c <- ['a' .. 'z']]

--- predefined SMTLib representations

--- predefined basic types and type constructors
predefTypes :: BM QName SMT.Ident
predefTypes = listToBM (<) (<) $ map (mapFst prel)
  [ ("Bool","Bool"), ("Int","Int"), ("Float","Float")
  , ("()","Unit"), ("[]","List"), ("(,)","Tuple2"), ("(,,)","Tuple3")
  , ("(,,,)","Tuple4"), ("(,,,,)","Tuple5"), ("(,,,,,)","Tuple6")
  , ("(,,,,,,)","Tuple7"), ("(,,,,,,,)","Tuple8"), ("(,,,,,,,,)","Tuple9")
  , ("(,,,,,,,,,)","Tuple10"), ("(,,,,,,,,,,)","Tuple11")
  , ("(,,,,,,,,,,,)","Tuple12"), ("(,,,,,,,,,,,,)","Tuple13")
  , ("(,,,,,,,,,,,,,)","Tuple14"), ("(,,,,,,,,,,,,,,)","Tuple15")
  ]

--- predefined constructors
predefCons :: BM SymObj SMT.Ident
predefCons = listToBM (<) (<) $
  [ (prelSymCons "False" boolType,"false")
  , (prelSymCons "True"  boolType,"true")
  , (prelSymCons "[]" (listType (TVar 0)),"nil")
  , (prelSymCons ":" (mkFunType [TVar 0, listType (TVar 0)] (listType (TVar 0))),"insert")
  , (prelSymCons "()" unitType,"unit")
  ] ++ map genTpl [2..15]

--- data types which are ignored regarding the generation of SMT data type declarations
ignoredTypes :: [String]
ignoredTypes =  ["Bool", "Int", "Float", "Char", "_Dict", "IO", "[]", "(->)"]

-- helper

-- Generate mapping of symbolic FlatCurry constructor to SMT-LIB constructor for tuples
genTpl :: Int -> (SymObj, SMT.Ident)
genTpl n = (mkTplType ('(' : replicate (n-1) ',' ++ ")") n, "tuple" ++ show n)

-- Smart constructor for symbolic tuples
mkTplType :: String -> Int -> SymObj
mkTplType n a | a >= 2 = SymCons qn (mkFunType tvars (TCons qn tvars))
  where qn    = prel n
        tvars = map TVar [0 .. a-1]
