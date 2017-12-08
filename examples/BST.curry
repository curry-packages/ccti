module BST where

data SearchTree = Empty | Node Int SearchTree SearchTree
  deriving (Eq, Show)

mirror :: SearchTree -> SearchTree
mirror t = case t of
  Empty      -> Empty
  Node x l r -> Node x (mirror r) (mirror l)

insert :: Int -> SearchTree -> SearchTree
insert x Empty = Node x Empty Empty
insert x (Node n l r)
  | x == n    = Node n l            r
  | x <  n    = Node n (insert x l) r
  | otherwise = Node n l            (insert x r)

isElem :: Int -> SearchTree -> Bool
isElem _ Empty = False
isElem x (Node n l r)
  | x == n    = True
  | x <  n    = isElem x l
  | otherwise = isElem x r

delete :: Int -> SearchTree -> SearchTree
delete _ Empty = Empty
delete x (Node n l r)
  | x == n    = let (m, t) = splitMin r in Node m l t
  | x <  n    = Node n (delete x l) r
  | otherwise = Node n l            (delete x r)

splitMin :: SearchTree -> (Int, SearchTree)
splitMin Empty        = error "splitMin: empty search tree"
splitMin (Node n l r) = case l of
  Empty -> (n, r)
  _     -> let (m, t) = splitMin l in (m, Node n t r)

mirrorTwiceEq :: SearchTree -> Bool
mirrorTwiceEq t = t == (mirror (mirror t))

-- Test if given tree is a binary search tree within given boundaries
isBST :: Int -> Int -> SearchTree -> Bool
isBST low high t = case t of
  Empty      -> True
  Node n l r -> low < n && n < high
    && isBST low high l
    && isBST low high r

main :: SearchTree
main = mirror Empty
