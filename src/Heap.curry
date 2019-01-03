--- ----------------------------------------------------------------------------
--- This module defines a heap structure, containing bindings from variables
--- to markers for free variables or black holes, or the expressions.
---
--- @author  Björn Peemöller, Jan Tikovsky
--- @version October 2017
--- ----------------------------------------------------------------------------
module Heap where

import Data.FiniteMap
import FlatCurry.Annotated.Pretty (ppExp, ppVarIndex)
import FlatCurry.Annotated.Types
import Text.Pretty

import FlatCurryGoodies (TypeAnn)
import Substitution

--- A 'Binding' represents the value of a variable bound in the heap.
--- @cons BlackHole   - the variable is a black hole, such as in `let x = x in x`
--- @cons BoundVar e  - the variable is bound to an expression `e`
--- @cons LazyBound e - the variable is bound to an expression `e` lazily
--- @cons FreeVar     - the variable is a logic (free) variable
--- @cons LazyFree    - the variable is a lazily bound logic (free) variable
--- @cons SymArg      - the variable is a symbolic argument
data Binding = BlackHole
             | BoundVar  (AExpr TypeAnn)
             | LazyBound (AExpr TypeAnn)
             | FreeVar
             | LazyFree
             | SymArg
 deriving (Eq, Show)

--- A `Heap` is an association from a `VarIndex` to a `Binding`
type Heap = FM VarIndex Binding

--- Create an empty heap
emptyH :: Heap
emptyH = emptyFM (<)

--- Create a heap from a list of bindings
fromListH :: [(VarIndex, Binding)] -> Heap
fromListH = listToFM (<)

--- Check if heap is empty
isEmptyH :: Heap -> Bool
isEmptyH = isEmptyFM

--- Check if there is a binding for a variable in the heap
elemH :: VarIndex -> Heap -> Bool
elemH = elemFM

--- Lookup a binding for a variable in the heap
lookupH :: VarIndex -> Heap -> Maybe Binding
lookupH = flip lookupFM

--- Bind a variable to a binding in the given heap
bindH :: VarIndex -> Binding -> Heap -> Heap
bindH v b h = addToFM h v b

--- Bind a variable as a "black hole" in the given heap
bindHole :: VarIndex -> Heap -> Heap
bindHole v = bindH v BlackHole

--- Bind a variable to the given expression in the given heap
bindExpr :: VarIndex -> AExpr TypeAnn -> Heap -> Heap
bindExpr v e = bindH v (BoundVar e)

--- Bind a variable lazily to the given expression in the given heap
bindLazyExpr :: VarIndex -> AExpr TypeAnn -> Heap -> Heap
bindLazyExpr v e = bindH v (LazyBound e)

--- Bind a variable as "free" in the given heap
bindFree :: VarIndex -> Heap -> Heap
bindFree v = bindH v FreeVar

--- Bind a variable lazily as "free" in the given heap
bindLazyFree :: VarIndex -> Heap -> Heap
bindLazyFree v = bindH v LazyFree

--- Bind a variable as symbolic argument
bindSym :: VarIndex -> Heap -> Heap
bindSym v = bindH v SymArg

--- Unbind a variable in the given heap
unbind :: VarIndex -> Heap -> Heap
unbind = flip delFromFM

--- Pretty printing

instance Pretty Binding where
  pretty BlackHole     = text "\x25a0"
  pretty (BoundVar  e) = ppExp e
  pretty (LazyBound e) = text "~" <> ppExp e
  pretty FreeVar       = text "free"
  pretty LazyFree      = text "~free"
  pretty SymArg        = text "sym"

ppHeap :: Heap -> Doc
ppHeap h = ppHeap' $ fmToList h
  where
  ppHeap' []     = text "[]"
  ppHeap' heap@(_:_) = listSpaced $ map ppBinding heap
    where ppBinding (i, b) = ppVarIndex i <+> char '\x21a6' <+> pretty b
