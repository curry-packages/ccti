module Search.DFS where

import PriorityQueue (PQueue, addElem, emptyPQ, getElem, rmvElem)
import Symbolic      (Depth, SymNode (..))

type SymTree = PQueue Depth SymNode

--- Create a new empty symbolic tree
newTree :: SymTree
newTree = emptyPQ

--- Get the next node according to the search strategy
nextNode :: SymTree -> Maybe SymNode
nextNode = getElem

--- Add a node to given symbolic tree
addNode :: SymNode -> SymTree -> SymTree
addNode n@(SymNode d _ _ _ _ _) t = addElem d n t

--- Delete a node in the given symbolic tree
delNode :: Depth -> SymTree -> SymTree
delNode = rmvElem
