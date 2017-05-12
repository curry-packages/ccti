--- ----------------------------------------------------------------------------
--- This module defines a heap structure, containing bindings from variables
--- to markers for free variables or black holes, or the expressions.
---
--- @author  Björn Peemöller, Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module Heap where

import FiniteMap
import FlatCurry.Annotated.Types

import PrettyPrint

--- A 'Binding' represents the value of a variable bound in the heap.
--- @cons BlackHole   - the variable is a black hole, such as in `let x = x in x`
--- @cons BoundVar e  - the variable is bound to an expression `e`
--- @cons LazyBound e - the variable is bound to an expression `e` lazily
--- @cons FreeVar     - the variable is a logic (free) variable
--- @cons LazyFree    - the variable is a lazily bound logic (free) variable
data Binding = BlackHole
             | BoundVar  (AExpr TypeExpr)
             | LazyBound (AExpr TypeExpr)
             | FreeVar
             | LazyFree
 deriving Show

--- A `Heap` is an association from a `VarIndex` to a `Binding`
type Heap = FM VarIndex Binding

--- Create an empty heap
emptyH :: Heap
emptyH = emptyFM (<)

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
bindExpr :: VarIndex -> AExpr TypeExpr -> Heap -> Heap
bindExpr v e = bindH v (BoundVar e)

--- Bind a variable lazily to the given expression in the given heap
bindLazyExpr :: VarIndex -> AExpr TypeExpr -> Heap -> Heap
bindLazyExpr v e = bindH v (LazyBound e)

--- Bind a variable as "free" in the given heap
bindFree :: VarIndex -> Heap -> Heap
bindFree v = bindH v FreeVar

--- Bind a variable lazily as "free" in the given heap
bindLazyFree :: VarIndex -> Heap -> Heap
bindLazyFree v = bindH v LazyFree

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

ppHeap :: Heap -> Doc
ppHeap h = ppHeap' $ fmToList h
  where
  ppHeap' []     = text "[]"
  ppHeap' heap@(_:_) = listSpaced $ map ppBinding heap
    where ppBinding (i, b) = ppVarIndex i <+> char '\x21a6' <+> pretty b
