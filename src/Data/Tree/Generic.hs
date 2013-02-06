{-# LANGUAGE TypeFamilies #-}

module Data.Tree.Generic (
      A.empty
    , lookup
    , fromSortedList
    , fromList
    , insert
    ) where

import Prelude hiding (lookup)
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Functor.Foldable

import qualified Data.Tree.Algorithms as A

lookup :: (Ord k, Foldable t, Base t ~ A.TreeNode k v) => k -> t -> Maybe v
lookup = cata . A.lookup
{-# INLINE lookup #-}

fromSortedList :: (Unfoldable t, Base t ~ A.TreeNode k v) => [(k, v)] -> t
fromSortedList = ana A.fromSortedList
{-# INLINE fromSortedList #-}

fromList :: (Ord k, Unfoldable t, Base t ~ A.TreeNode k v) => [(k, v)] -> t
fromList = fromSortedList . sortBy (comparing fst)
{-# INLINE fromList #-}

insert :: (Ord k, Foldable t, Unfoldable t, Base t ~ A.TreeNode k v) => k -> v -> t -> t
insert k v = apo (A.insert k v)
{-# INLINE insert #-}
