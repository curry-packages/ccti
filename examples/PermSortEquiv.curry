-- Check this program with
--
--    curry check PermSortEquiv.curry -m 250

import Nat
import Test.Prop

psort :: Ord a => [a] -> [a]
psort xs | sorted ys = ys where ys = perm xs

perm []     = []
perm (x:xs) = ndinsert x (perm xs)
  where ndinsert x ys     = x : ys
        ndinsert x (y:ys) = y : ndinsert x ys

sorted []       = True
sorted [_]      = True
sorted (x:y:ys) = x<=y & sorted (y:ys)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort (filter (<x) xs) ++ [x] ++ qsort (filter (>=x) xs)

-- this sort operation is too lazy
-- lazily constructs list which are only partially sorted (in contrast to psort)
-- e.g. idsort [2,3,4,1] -> 2 : idSorted [3,4,1] -> 2 : 3 : idSorted [4,1]
isort :: Ord a => [a] -> [a]
isort xs = idSorted (perm xs)
 where idSorted []              = []
       idSorted [x]             = [x]
       idSorted (x:y:ys) | x<=y = x : idSorted (y:ys)


-- Our PADL'12 paper defines: if E[f1] and E[f2] reduces to the same
-- partial values, then f1 and f2 are equivalent.

-- Therefore, we check this property:

data Partial_List a = Bot_List | Empty | Cons a (Partial_List a)
 deriving (Eq, Ord, Show, Read)

data Partial_Int = Bot_Int | DefInt Int
 deriving (Eq, Ord, Show, Read)

data Partial_Ordering = Bot_Ordering | P_LT | P_EQ | P_GT
 deriving (Eq, Ord, Show, Read)

peval_Int :: Int -> Partial_Int -> Partial_Int
peval_Int _ Bot_Int    = Bot_Int
peval_Int i (DefInt i) = DefInt i

peval_Ordering :: Ordering -> Partial_Ordering -> Partial_Ordering
peval_Ordering _  Bot_Ordering = Bot_Ordering
peval_Ordering LT P_LT         = P_LT
peval_Ordering EQ P_EQ         = P_EQ
peval_Ordering GT P_GT         = P_GT

peval_List :: (a -> pa -> pa) -> [a] -> Partial_List pa -> Partial_List pa
peval_List _    _      Bot_List    = Bot_List
peval_List _    []     Empty       = Empty
peval_List flim (x:xs) (Cons y ys) = Cons (flim x y) (peval_List flim xs ys)

-- in PAKCS, the counter example is reported by 218th test:
psort_equiv_isort p x = peval_List peval_Ordering (psort x) p <~>
                        peval_List peval_Ordering (isort x) p

-- in PAKCS, the counter example is reported by 218th test:
qsort_equiv_isort p x = peval_List peval_Ordering (qsort x) p <~>
                        peval_List peval_Ordering (isort x) p
