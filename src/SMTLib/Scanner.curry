--- ----------------------------------------------------------------------------
--- This module provides a simple scanner for the SMT-LIB language.
---
--- @author  Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module SMTLib.Scanner where

import Char
import FiniteMap (FM, listToFM, lookupFM)

data Token   -- symbols
           = LParen
           | RParen
           | Semi
           | Bang
           | Underscore
           | Colon
           | DQuote
             -- reserved keywords
           | KW_as
           | KW_BINARY
           | KW_DECIMAL
           | KW_error
           | KW_exists
           | KW_HEXADEC
           | KW_forall
           | KW_let
           | KW_model   -- not part of the SMTLib standard
           | KW_NUMERAL
           | KW_par
           | KW_sat
           | KW_STRING
           | KW_success
           | KW_unknown
           | KW_unsat
           | KW_unsupported
             -- command names
           | KW_assert
           | KW_check_sat
           | KW_check_sat_assuming
           | KW_decl_const
           | KW_decl_datatypes -- not part of the SMTLib standard
           | KW_decl_fun
           | KW_decl_sort
           | KW_def_fun
           | KW_def_fun_rec
           | KW_def_funs_rec
           | KW_def_sort
           | KW_echo
           | KW_exit
           | KW_get_assertions
           | KW_get_assignment
           | KW_get_info
           | KW_get_model
           | KW_get_option
           | KW_get_proof
           | KW_get_unsat_assumptions
           | KW_get_unsat_core
           | KW_get_value
           | KW_pop
           | KW_push
           | KW_reset
           | KW_reset_assertions
           | KW_set_info
           | KW_set_logic
           | KW_set_option
             -- info responses
           | KW_assertion_stack_levels
           | KW_authors
           | KW_error_behavior
           | KW_name
           | KW_reason_unknown
           | KW_version
             -- error behaviors
           | KW_immediate_exit
           | KW_continued_execution
             -- reason unknown
           | KW_memout
           | KW_incomplete
             -- identifier, numeral, bvalue
           | Id String
           | Num Int
           | BVal Bool
  deriving (Eq, Show)

keywords :: FM String Token
keywords = listToFM (<)
  [ ("as", KW_as)
  , ("BINARY", KW_BINARY)
  , ("DECIMAL", KW_DECIMAL)
  , ("error", KW_error)
  , ("exists", KW_exists)
  , ("HEXADECIMAL", KW_HEXADEC)
  , ("forall", KW_forall)
  , ("let", KW_let)
  , ("model", KW_model)
  , ("NUMERAL", KW_NUMERAL)
  , ("par", KW_par)
  , ("sat", KW_sat)
  , ("STRING", KW_STRING)
  , ("success", KW_success)
  , ("unknown", KW_unknown)
  , ("unsat", KW_unsat)
  , ("unsupported", KW_unsupported)
  -- commands
  , ("assert", KW_assert)
  , ("check-sat", KW_check_sat)
  , ("check-sat-assuming", KW_check_sat_assuming)
  , ("declare-const", KW_decl_const)
  , ("declare-datatypes", KW_decl_datatypes)
  , ("declare-fun", KW_decl_fun)
  , ("declare-sort", KW_decl_sort)
  , ("define-fun", KW_def_fun)
  , ("define-fun-rec", KW_def_fun_rec)
  , ("define-funs-rec", KW_def_funs_rec)
  , ("define-sort", KW_def_sort)
  , ("echo", KW_echo)
  , ("exit", KW_exit)
  , ("get-assertions", KW_get_assertions)
  , ("get-assignment", KW_get_assignment)
  , ("get-info", KW_get_info)
  , ("get-model", KW_get_model)
  , ("get-option", KW_get_option)
  , ("get-proof", KW_get_proof)
  , ("get-unsat-assumptions", KW_get_unsat_assumptions)
  , ("get-unsat-core", KW_get_unsat_core)
  , ("get-value", KW_get_value)
  , ("pop", KW_pop)
  , ("push", KW_push)
  , ("reset", KW_reset)
  , ("reset-assertions", KW_reset_assertions)
  , ("set-info", KW_set_info)
  , ("set-logic", KW_set_logic)
  , ("set-option", KW_set_option)
  -- info responses
  , ("assertion-stack-levels", KW_assertion_stack_levels)
  , ("authors", KW_authors)
  , ("error-behavior", KW_error_behavior)
  , ("name", KW_name)
  , ("reason-unknown", KW_reason_unknown)
  , ("version", KW_version)
  -- error behaviors
  , ("immediate-exit", KW_immediate_exit)
  , ("continued-execution", KW_continued_execution)
  -- reason unknown
  , ("memout", KW_memout)
  , ("incomplete", KW_incomplete)
  ]

scan :: String -> [Token]
scan ""     = []
scan s@(c:cs) = case c of
  '('                           -> LParen     : scan cs
  ')'                           -> RParen     : scan cs
  '!'                           -> Bang       : scan cs
  '_'                           -> Underscore : scan cs
  ':'                           -> Colon      : scan cs
  '"'                           -> DQuote     : scan cs
  ';'                           -> scanLineCmt cs
  _ | isNumber c                -> scanNum s
    | isAlpha  c || isSpecial c -> scanKWOrId s
    | isSpace  c                -> scan cs
    | otherwise                 -> error $ "Scanner.scan: Unexpected character " ++ [c]

isNumber :: Char -> Bool
isNumber c = isDigit c || c == '-'

isSpecial :: Char -> Bool
isSpecial c = c `elem` [ '+', '-', '/', '*', '=', '!', '.', '$', '_', '~', '&'
                       , '^', '<', '>', '@'
                       ]

--- line comments are ignored by the scanner
scanLineCmt :: String -> [Token]
scanLineCmt ""     = []
scanLineCmt (c:cs) = case c of
  '\n' -> scan cs
  _    -> scanLineCmt cs

scanNum :: String -> [Token]
scanNum cs = let (n, rest) = span isNumber cs
             in Num (read n) : scan rest

scanKWOrId :: String -> [Token]
scanKWOrId cs = let (i, rest) = span (\c -> isAlphaNum c || isSpecial c) cs
                in kwOrId i : scan rest

kwOrId :: String -> Token
kwOrId s = maybe (idOrBVal s) id (lookupFM keywords s)

idOrBVal :: String -> Token
idOrBVal s = case s of
  "true"  -> BVal True
  "false" -> BVal False
  _       -> Id s
