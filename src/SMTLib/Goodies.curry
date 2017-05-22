--- ----------------------------------------------------------------------------
--- This module provides some goodies and utility functions for SMTLib.
---
--- @author  Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module SMTLib.Goodies where

import FlatCurry.Types (VarIndex)

import SMTLib.Types

--- Transform a FlatCurry variable index into an SMTLib symbol
var2SMT :: VarIndex -> Symbol
var2SMT vi = 'x' : show (abs vi)

--- smart constructors for SMT terms

--- smart constructor for numeral SMT terms
tnum :: Int -> Term
tnum = TConst . Num

--- smart constructor for variables
tvar :: VarIndex -> Term
tvar vi = tcomb (var2SMT vi) []

--- smart constructor for constructor terms
tcomb :: Ident -> [Term] -> Term
tcomb i ts = TComb (Id i) ts

--- smart constructor for qualified constructor terms
qtcomb :: Ident -> Sort -> [Term] -> Term
qtcomb i s ts = TComb (As i s) ts

--- smart constructors for SMT sorts

--- smart constructor for a sort representing type variables
tyVar :: Sort
tyVar = SComb "_TVar" []

--- smart constructor for a sort representing a functional type
tyFun :: Sort
tyFun = SComb "_Fun" []

--- smart constructor for sorts
tyComb :: Ident -> [Sort] -> Sort
tyComb i ss = SComb i ss

--- smart constructor for an equational SMT term
(=%) :: Term -> Term -> Term
t1 =% t2 = tcomb "=" [t1, t2]

--- smart constructor for an inequational SMT term
(/=%) :: Term -> Term -> Term
t1 /=% t2 = tcomb "not" [tcomb "=" [t1, t2]]
