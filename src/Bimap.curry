module Bimap where

import FiniteMap
import Utils     (swap)

--- bidirectional map
data BM a b = BM (FM a b) (FM b a)

emptyBM :: (a -> a -> Bool) -> (b -> b -> Bool) -> BM a b
emptyBM cmp1 cmp2 = BM (emptyFM cmp1) (emptyFM cmp2)

unitBM :: (a -> a -> Bool) -> (b -> b -> Bool) -> a -> b -> BM a b
unitBM cmp1 cmp2 k v = BM (unitFM cmp1 k v) (unitFM cmp2 v k)

listToBM :: (Eq a, Eq b) => (a -> a -> Bool) -> (b -> b -> Bool) -> [(a, b)] -> BM a b
listToBM cmp1 cmp2 kvs = BM (listToFM cmp1 kvs) (listToFM cmp2 (map swap kvs))

addToBM :: (Eq a, Eq b) => a -> b -> BM a b -> BM a b
addToBM k v (BM fm1 fm2) = BM (addToFM fm1 k v) (addToFM fm2 v k)

addListToBM :: (Eq a, Eq b) => [(a, b)] -> BM a b -> BM a b
addListToBM kvs (BM fm1 fm2) = BM (addListToFM fm1 kvs) (addListToFM fm2 (map swap kvs))

addToBM_C :: (Eq a, Eq b) => (b -> b -> b) -> (a -> a -> a) -> a -> b -> BM a b -> BM a b
addToBM_C cmb1 cmb2 k v (BM fm1 fm2) = BM (addToFM_C cmb1 fm1 k v) (addToFM_C cmb2 fm2 v k)

addListToBM_C :: (Eq a, Eq b) => (b -> b -> b) -> (a -> a -> a) -> [(a, b)] -> BM a b -> BM a b
addListToBM_C cmb1 cmb2 kvs (BM fm1 fm2) = BM (addListToFM_C cmb1 fm1 kvs)
                                              (addListToFM_C cmb2 fm2 (map swap kvs))

delFromBM :: (Eq a, Eq b) => a -> BM a b -> BM a b
delFromBM = delete . Left

delFromBMR :: (Eq a, Eq b) => b -> BM a b -> BM a b
delFromBMR = delete . Right

delete :: (Eq a, Eq b) => Either a b -> BM a b -> BM a b
delete e (BM fm1 fm2) = BM (perhaps (flip delFromFM) x fm1)
                           (perhaps (flip delFromFM) y fm2)
  where
  perhaps = maybe id
  x = either Just (lookupFM fm2) e
  y = either (lookupFM fm1) Just e

delListFromBM :: (Eq a, Eq b) => [a] -> BM a b -> BM a b
delListFromBM ks bm = foldl (flip delFromBM) bm ks

delListFromBMR :: (Eq a, Eq b) => [b] -> BM a b -> BM a b
delListFromBMR ks bm = foldl (flip delFromBMR) bm ks

updBM :: (Eq a, Eq b) => (b -> b) -> a -> BM a b -> BM a b
updBM f k bm@(BM fm1 fm2) = case lookupFM fm1 k of
  Nothing -> bm
  Just v  -> BM (updFM fm1 k f) (addToFM (delFromFM fm2 v) (f v) k)

updBMR :: (Eq a, Eq b) => (a -> a) -> b -> BM a b -> BM a b
updBMR f k bm@(BM fm1 fm2) = case lookupFM fm2 k of
  Nothing -> bm
  Just v  -> BM (addToFM (delFromFM fm1 v) (f v) k) (updFM fm2 k f)

sizeBM :: BM a b -> Int
sizeBM (BM fm1 _) = sizeFM fm1

isEmptyBM :: BM a b -> Bool
isEmptyBM (BM fm1 _) = isEmptyFM fm1

elemBM :: (Eq a, Eq b) => a -> BM a b -> Bool
elemBM k (BM fm1 _) = elemFM k fm1

elemBMR :: (Eq a, Eq b) => b -> BM a b -> Bool
elemBMR k (BM _ fm2) = elemFM k fm2

lookupBM :: (Eq a, Eq b) => a -> BM a b -> Maybe b
lookupBM k (BM fm1 _) = lookupFM fm1 k

lookupBMR :: (Eq a, Eq b) => b -> BM a b -> Maybe a
lookupBMR k (BM _ fm2) = lookupFM fm2 k

lookupWithDefaultBM :: (Eq a, Eq b) => b -> a -> BM a b -> b
lookupWithDefaultBM def k (BM fm1 _) = lookupWithDefaultFM fm1 def k

lookupWithDefaultBMR :: (Eq a, Eq b) => a -> b -> BM a b -> a
lookupWithDefaultBMR def k (BM _ fm2) = lookupWithDefaultFM fm2 def k

bmToLists :: BM a b -> ([(a, b)], [(b, a)])
bmToLists (BM fm1 fm2) = (fmToList fm1, fmToList fm2)

keysBM :: BM a b -> [a]
keysBM (BM fm1 _) = keysFM fm1

eltsBM :: BM a b -> [b]
eltsBM (BM fm1 _) = eltsFM fm1
