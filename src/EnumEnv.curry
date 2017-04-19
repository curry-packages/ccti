module EnumEnv where

import FlatCurry.Pretty (defaultOptions, ppQName)
import FlatCurry.Types
import FiniteMap

import PrettyPrint
import Symbolic         (ConsNr, NthOfM (..))
import Utils

type EnumEnv = FM QName ConsNr

emptyEEnv :: EnumEnv
emptyEEnv = emptyFM (<)

--- enumeration monad
data Enum a = Enum { runEnum :: EnumEnv -> (a, EnumEnv) }

instance Monad Enum where
  return x = Enum $ \s -> (x, s)

  (Enum f) >>= g = Enum $ \s -> let (x, s') = f s in (runEnum (g x)) s'

modify :: (EnumEnv -> EnumEnv) -> Enum ()
modify f = Enum $ \s -> ((), f s)

insertEnum :: QName -> ConsNr -> Enum ()
insertEnum qn cnr = modify (\env -> addToFM env qn cnr)

lookupEnum :: QName -> EnumEnv -> Maybe ConsNr
lookupEnum = flip lookupFM

enumerate :: [TypeDecl] -> EnumEnv
enumerate tys = snd $ (runEnum (mapM_ enumType tys)) emptyEEnv

enumType :: TypeDecl -> Enum ()
enumType (Type   _ _ _ cs) = zipWithM_ (enumCons (length cs)) [1 ..] cs
enumType (TypeSyn _ _ _ _) = return ()

enumCons :: Int -> Int -> ConsDecl -> Enum ()
enumCons s nr (Cons qn _ _ _) = insertEnum qn (nr :/: s)

--- Pretty printing

ppEEnv :: EnumEnv -> Doc
ppEEnv eenv = ppEEnv' $ fmToList eenv
  where
  ppEEnv' []        = text "[]"
  ppEEnv' env@(_:_) = listSpaced $ map ppEnum env
    where ppEnum (qn, cnr) = ppQName defaultOptions qn
                          <+> char '\x21a6'
                          <+> pretty cnr
