--- ----------------------------------------------------------------------------
--- Difference list implementation for Curry
---
--- @author  Jan Tikovsky
--- @version October 2017
--- ----------------------------------------------------------------------------
module Data.DList where

import List (nub)

data DList a = DL { unDL :: [a] -> [a] }

--- Create an empty diff list
empty :: DList a
empty = DL id

--- Create a diff list with a single element
singleton :: a -> DList a
singleton = DL . (:)

--- Prepend an element to a diff list
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)

--- Append two diff lists
append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

--- Convert a list to a diff list
fromList :: [a] -> DList a
fromList = DL . (++)

--- Convert a diff list to a list
toList :: DList a -> [a]
toList = ($ []) . unDL

--- Convert a diff list to a duplicate free list
toListNub :: Eq a => DList a -> [a]
toListNub = nub . toList

--- Get the head element of a diff list
hd :: DList a -> a
hd xs = case toList xs of
  []    -> error "Data.DList.hd: List is empty"
  (y:_) -> y

--- Get the tail of a diff list
tl :: DList a -> DList a
tl xs = case toList xs of
  []     -> error "Data.DList.hd: List is empty"
  (_:ys) -> fromList ys

--- Show instance
instance Show a => Show (DList a) where
  show = show . toList
