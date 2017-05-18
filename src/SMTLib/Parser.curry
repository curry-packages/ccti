--- ----------------------------------------------------------------------------
--- This module provides parser functions for the SMT-LIB language.
--- Currently there are only parsers for a subset of the language,
--- namely command responses.
---
--- @author  Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module SMTLib.Parser where

import           ParserComb
import           SMTLib.Scanner
import qualified SMTLib.Types   as SMT

type SMTParser a = Parser Token a

parseCmdRsp :: String -> SMT.CmdResponse
parseCmdRsp str = case (runParser parseResponse . scan) str of
  Left  msg       -> error msg
  Right ([], rsp) -> rsp
  Right (ts,   _) -> error $ "Incomplete parse: " ++ show ts

parseResponse :: SMTParser SMT.CmdResponse
parseResponse =  terminal KW_success     *> yield SMT.SuccessRsp
             <|> terminal KW_sat         *> yield (SMT.CheckSatRsp SMT.Sat)
             <|> terminal KW_unsat       *> yield (SMT.CheckSatRsp SMT.Unsat)
             <|> terminal KW_unknown     *> yield (SMT.CheckSatRsp SMT.Unknown)
             <|> terminal KW_unsupported *> yield SMT.UnsupportedRsp
             <|> terminal LParen *> parseErrorRsp         <* terminal RParen
             <|> terminal LParen *> parseGetValueRsp      <* terminal RParen
             <|> terminal LParen *> parseGetModelRsp      <* terminal RParen
             <|> terminal LParen *> parseGetAssertionsRsp <* terminal RParen
             <|> terminal LParen *> parseGetAssignmentRsp <* terminal RParen
             <|> terminal LParen *> parseGetInfoRsp       <* terminal RParen
             <|> yield SMT.EchoRsp <*> parseStr
             <|> yield SMT.GetOptionRsp <*> parseAttrValue
             <|> yield SMT.GetProofRsp <*> parseSExpr
             -- the get-unsat-assumptions-response and the get-unsat-core-response overlap
             <|> yield SMT.GetUnsatAssumptionsRsp <*> many parseSym
             <|> yield SMT.GetUnsatCoreRsp <*> many parseSym

--- parser for get-assertions-response
parseGetAssertionsRsp :: SMTParser SMT.CmdResponse
parseGetAssertionsRsp = yield SMT.GetAssertionsRsp <*> many parseTerm

--- parser for get-assignment-response
parseGetAssignmentRsp :: SMTParser SMT.CmdResponse
parseGetAssignmentRsp = yield SMT.GetAssignmentRsp <*> many parseTValPair

--- parser for a t-valuation pair
parseTValPair :: SMTParser (SMT.Symbol, Bool)
parseTValPair =
  terminal LParen *> yield (,) <*> parseSym <*> parseBool <* terminal RParen

--- parser for get-info-response
parseGetInfoRsp :: SMTParser SMT.CmdResponse
parseGetInfoRsp = yield SMT.GetInfoRsp <*> some parseInfoRsp

--- parser for an info response
parseInfoRsp :: SMTParser SMT.InfoRsp
parseInfoRsp
  =  terminal Colon *> terminal KW_assertion_stack_levels
                    *> yield SMT.AssertionStackLevelsRsp <*> parseNum
 <|> terminal Colon *> terminal KW_authors *> yield SMT.AuthorsRsp <*> parseStr
 <|> terminal Colon *> terminal KW_error_behavior *> yield SMT.ErrorBehaviorRsp
                   <*> parseErrorBehavior
 <|> terminal Colon *> terminal KW_name *> yield SMT.NameRsp <*> parseStr
 <|> terminal Colon *> terminal KW_reason_unknown *> yield SMT.ReasonUnknownRsp
                   <*> parseReasonUnknown
 <|> terminal Colon *> terminal KW_version *> yield SMT.VersionRsp <*> parseStr
 <|> yield SMT.AttrRsp <*> parseAttribute

--- parser for error behavior
parseErrorBehavior :: SMTParser SMT.ErrorBehavior
parseErrorBehavior
  =  terminal KW_immediate_exit *> yield SMT.ImmediateExit
 <|> terminal KW_continued_execution *> yield SMT.ContinuedExecution

--- parser for reason unknown
parseReasonUnknown :: SMTParser SMT.ReasonUnknown
parseReasonUnknown =  terminal KW_memout *> yield SMT.Memout
                  <|> terminal KW_incomplete *> yield SMT.Incomplete
                  <|> yield SMT.SEReason <*> parseSExpr

--- parser for a get-model-response
parseGetModelRsp :: SMTParser SMT.CmdResponse
parseGetModelRsp =
  -- Note: the keyword 'model' is not part of the SMTLib standard (but used in Z3)
  terminal KW_model *> yield SMT.GetModelRsp <*> many parseModelRsp

--- parser for a model response
parseModelRsp :: SMTParser SMT.ModelRsp
parseModelRsp
  =  terminal LParen *> terminal KW_def_fun *> yield SMT.MRFun <*> parseFunDef
                    <*  terminal RParen
 <|> terminal LParen *> terminal KW_def_fun_rec *> yield SMT.MRFunRec
                    <*> parseFunDef <* terminal RParen
 <|> terminal LParen *> terminal KW_def_funs_rec
                     *> yield ((SMT.MRFunsRec .) . zip) <*  terminal LParen
                    <*> some parseFunDec <* terminal RParen
                    <*  terminal LParen <*> some parseTerm   <* terminal RParen
                    <*  terminal RParen

--- parser for a function definition
parseFunDef :: SMTParser SMT.FunDef
parseFunDef = yield SMT.FunDef <*> parseSym <* terminal LParen
  <*> many parseSortedVar <* terminal RParen <*> parseSort <*> parseTerm

--- parser for a function declaration
parseFunDec :: SMTParser SMT.FunDec
parseFunDec
  = terminal LParen *> yield SMT.FunDec <*> parseSym <* terminal LParen
 <*> many parseSortedVar <* terminal RParen <*> parseSort <* terminal RParen

--- parser for a get-value-response
parseGetValueRsp :: SMTParser SMT.CmdResponse
parseGetValueRsp = yield SMT.GetValueRsp <*> some parseValPair

--- parser for an error response
parseErrorRsp :: SMTParser SMT.CmdResponse
parseErrorRsp =  terminal KW_error *> yield SMT.ErrorRsp <*> parseStr

--- parser for a boolean value
parseBool :: SMTParser Bool
parseBool = Parser $ \tokens -> case tokens of
  []          -> (runParser eof) tokens
  BVal b : ts -> Right (ts, b)
  t      : ts -> (runParser (unexpected t)) ts

--- parser for a numeral
parseNum :: SMTParser SMT.Numeral
parseNum = Parser $ \tokens -> case tokens of
  []         -> (runParser eof) tokens
  Num n : ts -> Right (ts, n)
  t     : ts -> (runParser (unexpected t)) ts

--- parser for a string
parseStr :: SMTParser String
parseStr = terminal DQuote *> parseSym <* terminal DQuote

--- parser for a symbol
parseSym :: SMTParser SMT.Symbol
parseSym = Parser $ \tokens -> case tokens of
  []        -> (runParser eof) tokens
  Id s : ts -> Right (ts, s)
  t    : ts -> (runParser (unexpected t)) ts

--- parser for a valuation pair
parseValPair :: SMTParser SMT.ValuationPair
parseValPair =
  terminal LParen *> yield (,) <*> parseTerm <*> parseTerm <* terminal RParen

--- parser for terms
parseTerm :: SMTParser SMT.Term
parseTerm = Parser $ \tokens -> case tokens of
  []          -> (runParser eof) tokens
  Num n  : ts -> Right (ts, SMT.TConst (SMT.Num n))
  Id  s  : ts -> Right (ts, SMT.TComb (SMT.Id s) [])
  LParen : ts -> (runParser parseParenTerm) ts
  t      : ts -> (runParser (unexpected t)) ts

--- parser for parenthesized terms
parseParenTerm :: SMTParser SMT.Term
parseParenTerm
     -- parser for qualified identifiers
  =  terminal KW_as *> yield SMT.TComb <*> (yield SMT.As <*> parseSym
                   <*> parseSort) <*> yield [] <* terminal RParen
     -- parser for complex terms
 <|> yield SMT.TComb <*> parseQIdent <*> some parseTerm <* terminal RParen
     -- parser for let terms
 <|> terminal KW_let *> yield SMT.Let <* terminal LParen <*> some parseVarBind
                    <* terminal RParen <*> parseTerm <* terminal RParen
     -- parser for universally quantified terms
 <|> terminal KW_forall *> yield SMT.Forall <* terminal LParen
                       <*> some parseSortedVar <* terminal RParen
                       <*> parseTerm <* terminal RParen
     -- parser for existentially quantified terms
 <|> terminal KW_exists *> yield SMT.Exists <* terminal LParen
                       <*> some parseSortedVar <* terminal RParen
                       <*> parseTerm <* terminal RParen
     -- parser for annotated terms
 <|> terminal Bang *> yield SMT.Annot <*> parseTerm <*> some parseAttribute
                  <* terminal RParen

--- parser for let bindings
parseVarBind :: SMTParser (SMT.Symbol, SMT.Term)
parseVarBind =
  terminal LParen *> yield (,) <*> parseSym <*> parseTerm <* terminal RParen

--- parser for sorted variables
parseSortedVar :: SMTParser SMT.SortedVar
parseSortedVar =
  terminal LParen *> yield SMT.SV <*> parseSym <*> parseSort <* terminal RParen

--- parser for attributes
parseAttribute :: SMTParser SMT.Attribute
parseAttribute = Parser $ \tokens -> case tokens of
  []                             -> (runParser eof) tokens
  Colon : (Id s : (RParen : ts)) -> Right (RParen : ts, SMT.AKW (SMT.KW s))
  Colon : (Id s           : ts)  -> (runParser (yield (SMT.AVal (SMT.KW s))
                                      <*> parseAttrValue)) ts
  t                       : ts   -> (runParser (unexpected t)) ts

--- parser for an attribute value
parseAttrValue :: SMTParser SMT.AttrValue
parseAttrValue = Parser $ \tokens -> case tokens of
  []          -> (runParser eof) tokens
  Num n  : ts -> Right (ts, SMT.AVConst (SMT.Num n))
  Id  s  : ts -> Right (ts, SMT.AVSym s)
  LParen : ts -> (runParser (yield SMT.AVSExpr <*> many parseSExpr
                   <* terminal RParen)) ts
  t      : ts -> (runParser (unexpected t)) ts

--- parser for `SExpr`
parseSExpr :: SMTParser SMT.SExpr
parseSExpr = Parser $ \tokens -> case tokens of
  []          -> (runParser eof) tokens
  Num n  : ts -> Right (ts, SMT.SEConst (SMT.Num n))
  Id  s  : ts -> Right (ts, SMT.SESym s)
  Colon  : ts -> (runParser (yield (SMT.SEKW . SMT.KW) <*> parseSym)) ts
  LParen : ts -> (runParser (yield SMT.SEList <*> many parseSExpr
                   <* terminal RParen)) ts
  t      : ts -> (runParser (unexpected t)) ts

--- parser for qualified identifiers
parseQIdent :: SMTParser SMT.QIdent
parseQIdent = Parser $ \tokens -> case tokens of
  []          -> (runParser eof) tokens
  Id  s  : ts -> Right (ts, SMT.Id s)
  LParen : ts -> (runParser (terminal KW_as *> yield SMT.As <*> parseSym
                   <*> parseSort <* terminal RParen)) ts
  t      : ts -> (runParser (unexpected t)) ts

--- parser for sorts
parseSort :: SMTParser SMT.Sort
parseSort = Parser $ \tokens -> case tokens of
  []          -> (runParser eof) tokens
  Id s   : ts -> Right (ts, SMT.SComb s [])
  LParen : ts -> (runParser (yield SMT.SComb <*> parseSym <*> some parseSort
                   <*  terminal RParen)) ts
  t      : ts -> (runParser (unexpected t)) ts




