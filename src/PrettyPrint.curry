--- ----------------------------------------------------------------------------
--- This module provides a type class for pretty printing.
---
--- @author  Jan Tikovsky
--- @version April 2017
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
