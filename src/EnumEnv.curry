--- ----------------------------------------------------------------------------
--- This module provides an environment for the enumeration of value
--- constructors.
--- For a type declaration like
---
---   data Bool = False | True
---
--- the following entries are added to the enumeration environment
---
---   [(False, (1,2)), (True, (2,2))]
---
--- @author  Jan Tikovsky
--- @version April 2017
--- ----------------------------------------------------------------------------
module EnumEnv where

import FlatCurry.Annotated.Pretty (defaultOptions, ppQName)
import FlatCurry.Annotated.Types
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
enumCons s nr (Cons qn _ _ _)
  | head (snd qn) == '_' = return ()                -- ignore dictionaries
  | otherwise            = insertEnum qn (nr :/: s)

--- Pretty printing

ppEEnv :: EnumEnv -> Doc
ppEEnv eenv = ppEEnv' $ fmToList eenv
  where
  ppEEnv' []        = text "[]"
  ppEEnv' env@(_:_) = listSpaced $ map ppEnum env
    where ppEnum (qn, cnr) = ppQName defaultOptions qn
                          <+> char '\x21a6'
                          <+> pretty cnr
