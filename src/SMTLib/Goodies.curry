--- ----------------------------------------------------------------------------
--- This module provides some goodies and utility functions for SMTLib.
---
--- @author  Jan Tikovsky
--- @version May 2017
--- ----------------------------------------------------------------------------
module SMTLib.Goodies where

import SMTLib.Types

--- smart constructor for numeral SMT terms
tnum :: Int -> Term
tnum = TCons . Num
