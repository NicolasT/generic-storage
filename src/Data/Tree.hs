module Data.Tree (
      Tree
    , empty
    , lookup
    , fromSortedList
    , fromList
    , insert
    ) where

import Prelude hiding (lookup)
import Data.Functor.Foldable (Fix)

import qualified Data.Tree.Generic as G
import qualified Data.Tree.Algorithms as A

type Tree k v = Fix (A.TreeNode k v)

empty :: Tree k v
empty = G.empty

lookup :: Ord k => k -> Tree k v -> Maybe v
lookup = G.lookup

fromSortedList :: [(k, v)] -> Tree k v
fromSortedList = G.fromSortedList

fromList :: Ord k => [(k, v)] -> Tree k v
fromList = G.fromList

insert :: Ord k => k -> v -> Tree k v -> Tree k v
insert = G.insert
