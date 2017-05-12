--- ----------------------------------------------------------------------------
--- This module provides a type class for pretty printing and bundles the
--- pretty printing libraries for Curry and annotated FlatCurry.
---
--- @author  Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module PrettyPrint ( module FlatCurry.Annotated.Pretty
                   , module Pretty
                   , module PrettyPrint) where

import FlatCurry.Annotated.Pretty hiding (indent)
import Pretty                     hiding (pretty)

class Pretty a where
  pretty :: a -> Doc

--- Pretty print the given documents separated with spaces and parenthesized
parent :: [Doc] -> Doc
parent = encloseSep lparen rparen space
