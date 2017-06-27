--- ----------------------------------------------------------------------------
--- This module provides a type class for pretty printing and bundles the
--- pretty printing libraries for Curry and annotated FlatCurry.
---
--- @author  Jan Tikovsky
--- @version June 2017
--- ----------------------------------------------------------------------------
module PrettyPrint ( module FlatCurry.Annotated.Pretty
                   , module Pretty
                   , module PrettyPrint) where

import FiniteMap                         (FM, fmToList)
import FlatCurry.Annotated.Pretty hiding (indent)
import FlatCurry.Annotated.Types
import Pretty                     hiding (pretty)

class Pretty a where
  pretty :: a -> Doc

--- Pretty print the given documents separated with spaces and parenthesized
parent :: [Doc] -> Doc
parent = encloseSep lparen rparen space

--- Pretty print finite map
ppFM :: ((a, b) -> Doc) -> FM a b -> Doc
ppFM ppEntry fm = listSpaced $ map ppEntry $ fmToList fm

--- Pretty printing of test cases
ppTestCase :: (AExpr TypeExpr, [AExpr TypeExpr]) -> Doc
ppTestCase (e, res) = parens (ppExp e <+> equals <+> set (map ppExp res))

