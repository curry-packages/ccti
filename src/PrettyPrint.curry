--- ----------------------------------------------------------------------------
--- This module provides a type class for pretty printing.
---
--- @author  Jan Tikovsky
--- @version April 2017
--- ----------------------------------------------------------------------------
module PrettyPrint ( module FlatCurry.Pretty
                   , module Pretty
                   , module PrettyPrint) where

import FlatCurry.Pretty hiding (indent)
import Pretty           hiding (pretty)

class Pretty a where
  pretty :: a -> Doc
