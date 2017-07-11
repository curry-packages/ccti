--- ----------------------------------------------------------------------------
--- This module provides some goodies and utility functions for SMTLib.
---
--- @author  Jan Tikovsky
--- @version July 2017
--- ----------------------------------------------------------------------------
module SMTLib.Goodies where

import FlatCurry.Types  (VarIndex, Literal (..))

import FlatCurryGoodies (LConstr (..))
import SMTLib.Types

--- Transform a FlatCurry variable index into an SMTLib symbol
var2SMT :: VarIndex -> Symbol
var2SMT vi = 'x' : show (abs vi)

--- smart constructors for SMT terms

--- smart constructor for literal terms
lit2SMT :: Literal -> Term
lit2SMT (Intc   i) = tint   i
lit2SMT (Floatc f) = tfloat f
lit2SMT (Charc  _) = error "SMTLib.Goodies.lit2SMT: Characters are not supported"

--- smart constructor for integer SMT terms
tint :: Int -> Term
tint = TConst . Num

--- smart constructor for floating point SMT terms
tfloat :: Float -> Term
tfloat = TConst . Dec

--- smart constructor for character SMT terms (represented as string)
tchar :: Char -> Term
tchar = TConst . Str . (: [])

--- smart constructor for variables
tvar :: VarIndex -> Term
tvar vi = tcomb (var2SMT vi) []

--- smart constructor for constructor terms
tcomb :: Ident -> [Term] -> Term
tcomb i ts = TComb (Id i) ts

--- smart constructor for qualified constructor terms
qtcomb :: QIdent -> [Term] -> Term
qtcomb qi ts = TComb qi ts

--- smart constructor for universally quantified terms
forAll :: [VarIndex] -> [Sort] -> Term -> Term
forAll vs ss t = case vs of
  [] -> t
  _  -> Forall (zipWith SV (map var2SMT vs) ss) t

--- smart constructors for SMT sorts

--- smart constructor for a sort representing type variables
tyVar :: Sort
tyVar = SComb "TVar" []

--- smart constructor for a sort representing a functional type
tyFun :: Sort
tyFun = SComb "Fun" []

--- smart constructor for sorts
tyComb :: Ident -> [Sort] -> Sort
tyComb i ss = SComb i ss

--- smart constructor for a negated SMT term
tneg :: Term -> Term
tneg t = tcomb "not" [t]

--- smart constructor for an equational SMT term
(=%) :: Term -> Term -> Term
t1 =% t2 = tcomb "=" [t1, t2]

--- smart constructor for an inequational SMT term
(/=%) :: Term -> Term -> Term
t1 /=% t2 = tcomb "not" [tcomb "=" [t1, t2]]

(<%) :: Term -> Term -> Term
t1 <% t2 = tcomb "<" [t1, t2]

(<=%) :: Term -> Term -> Term
t1 <=% t2 = tcomb "<=" [t1, t2]

(>%) :: Term -> Term -> Term
t1 >% t2 = tcomb ">" [t1, t2]

(>=%) :: Term -> Term -> Term
t1 >=% t2 = tcomb ">=" [t1, t2]

lcs2SMT :: LConstr -> Term -> Term -> Term
lcs2SMT E  = (=%)
lcs2SMT NE = (/=%)
lcs2SMT L  = (<%)
lcs2SMT LE = (<=%)
lcs2SMT G  = (>%)
lcs2SMT GE = (>=%)

--- smart constructor for conjunctions of constraints
tand :: [Term] -> Term
tand = tcomb "and"

--- Constrain an SMT variable to be distinct from the given SMT constructors
noneOf :: VarIndex -> VarIndex -> [(QIdent, [Sort])] -> [Term]
noneOf idx dv qis = snd $ foldr ineq (idx, []) qis
  where ineq (qi, ss) (vi, cs) =
          let vn = vi - length ss
              vs = [vi, vi - 1 .. vn + 1]
          in (vn, forAll vs ss (tvar dv /=% qtcomb qi (map tvar vs)) : cs)

--- Generate a `nop` command
nop :: Command
nop = Echo ""

--- Generate an `Assert` command for a given list of SMT terms
assert :: [Term] -> Command
assert ts = case ts of
  []  -> nop
  [t] -> Assert t
  _   -> Assert $ tand ts

--- Get the unqualified identifier of an `QIdent`
unqual :: QIdent -> Ident
unqual (Id   i) = i
unqual (As i _) = i

--- Is given command a declaration of an algebraic data type
isDeclData :: Command -> Bool
isDeclData cmd = case cmd of
  DeclareDatatypes _ _ _ -> True
  _                      -> False
