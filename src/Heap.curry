--- ----------------------------------------------------------------------------
--- This module defines a heap structure, containing bindings from variables
--- to markers for free variables or black holes, or the expressions.
---
--- @author  Björn Peemöller, Jan Tikovsky
--- @version March 2017
--- ----------------------------------------------------------------------------
module Heap where

import FiniteMap
import FlatCurry.Types

--- A 'Binding' represents the value of a variable bound in the heap.
--- @cons BlackHole   - the variable is a black hole, such as in `let x = x in x`
--- @cons BoundVar e  - the variable is bound to an expression `e`
--- @cons LazyBound e - the variable is bound to an expression `e` lazily
--- @cons FreeVar     - the variable is a logic (free) variable
--- @cons LazyFree    - the variable is a lazily bound logic (free) variable
data Binding = BlackHole
             | BoundVar  Expr
             | LazyBound Expr
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
bindExpr :: VarIndex -> Expr -> Heap -> Heap
bindExpr v e = bindH v (BoundVar e)

--- Bind a variable lazily to the given expression in the given heap
bindLazyExpr :: VarIndex -> Expr -> Heap -> Heap
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







