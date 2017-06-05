module Bimap where

import FiniteMap
import PrettyPrint

--- bidirectional map
data BM a b = BM (FM a b) (FM b a)

fmA :: BM a b -> FM a b
fmA (BM x _) = x

fmB :: BM a b -> FM b a
fmB (BM _ y) = y

emptyBM :: (a -> a -> Bool) -> (b -> b -> Bool) -> BM a b
emptyBM cmp1 cmp2 = BM (emptyFM cmp1) (emptyFM cmp2)

unitBM :: (a -> a -> Bool) -> (b -> b -> Bool) -> a -> b -> BM a b
unitBM cmp1 cmp2 k v = BM (unitFM cmp1 k v) (unitFM cmp2 v k)

listToBM :: (Eq a, Eq b) => (a -> a -> Bool) -> (b -> b -> Bool) -> [(a, b)] -> BM a b
listToBM cmp1 cmp2 kvs = addListToBM kvs (emptyBM cmp1 cmp2)

addToBM :: (Eq a, Eq b) => a -> b -> BM a b -> BM a b
addToBM k v bm = case deleteBd k v bm of
  BM fm1 fm2 -> BM (addToFM fm1 k v) (addToFM fm2 v k)

addListToBM :: (Eq a, Eq b) => [(a, b)] -> BM a b -> BM a b
addListToBM []           bm = bm
addListToBM ((k, v):kvs) bm = addToBM k v (addListToBM kvs bm)

-- TODO: implementation wrong: cmb functions need to be applied in both finite maps !!!
-- addToBM_C :: (Eq a, Eq b) => (b -> b -> b) -> (a -> a -> a) -> a -> b -> BM a b -> BM a b
-- addToBM_C cmb1 cmb2 k v (BM fm1 fm2) = BM (addToFM_C cmb1 fm1 k v) (addToFM_C cmb2 fm2 v k)
--
-- addListToBM_C :: (Eq a, Eq b) => (b -> b -> b) -> (a -> a -> a) -> [(a, b)] -> BM a b -> BM a b
-- addListToBM_C cmb1 cmb2 kvs (BM fm1 fm2) = BM (addListToFM_C cmb1 fm1 kvs)
--                                               (addListToFM_C cmb2 fm2 (map swap kvs))

delFromBM :: (Eq a, Eq b) => a -> BM a b -> BM a b
delFromBM = delete . Left

delFromBMR :: (Eq a, Eq b) => b -> BM a b -> BM a b
delFromBMR = delete . Right

--- Delete bidirectional
deleteBd :: (Eq a, Eq b) => a -> b -> BM a b -> BM a b
deleteBd k v = delFromBM k . delFromBMR v

delete :: (Eq a, Eq b) => Either a b -> BM a b -> BM a b
delete e (BM fm1 fm2) = BM (perhaps1 (flip delFromFM) x fm1)
                           (perhaps2 (flip delFromFM) y fm2)
  where
  -- TODO: polymorphic lets
  perhaps1 = maybe id
  perhaps2 = maybe id
  x = either Just (lookupFM fm2) e
  y = either (lookupFM fm1) Just e

delListFromBM :: (Eq a, Eq b) => [a] -> BM a b -> BM a b
delListFromBM ks bm = foldl (flip delFromBM) bm ks

delListFromBMR :: (Eq a, Eq b) => [b] -> BM a b -> BM a b
delListFromBMR ks bm = foldl (flip delFromBMR) bm ks

updBM :: (Eq a, Eq b) => (b -> b) -> a -> BM a b -> BM a b
updBM f k bm@(BM fm1 _) = case lookupFM fm1 k of
  Nothing -> bm
  Just v  -> addToBM k (f v) (deleteBd k v bm)

updBMR :: (Eq a, Eq b) => (a -> a) -> b -> BM a b -> BM a b
updBMR f k bm@(BM _ fm2) = case lookupFM fm2 k of
  Nothing -> bm
  Just v  -> addToBM (f v) k (deleteBd v k bm)

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

--- Pretty print bidirectional map
ppBM :: ((a, b) -> Doc) -> BM a b -> Doc
ppBM ppEntry (BM fm _) = ppFM ppEntry fm
