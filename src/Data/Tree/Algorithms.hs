{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TypeFamilies #-}

module Data.Tree.Algorithms (
      TreeNode
    , empty
    , lookup
    , fromSortedList
    , insert
    ) where

import Prelude hiding (lookup)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import Control.Functor.Algebra
import Data.Functor.Foldable

data TreeNode k v r = Leaf
                    | Branch k v r r
  deriving (Show, Eq, Functor, F.Foldable, Traversable)

empty :: (Unfoldable t, Base t ~ TreeNode k v) => t
empty = embed Leaf
{-# INLINE empty #-}

lookup :: Ord k => k -> Algebra (TreeNode k v) (Maybe v)
lookup k n = case n of
    Leaf -> Nothing
    Branch k' v l r -> case k `compare` k' of
        LT -> l
        EQ -> Just v
        GT -> r
{-# INLINE lookup #-}

fromSortedList :: Coalgebra (TreeNode k v) [(k, v)]
fromSortedList xs | null xs = Leaf
                  | otherwise = Branch k v l r
                      where
                        (l, (k, v) : r) = splitAt (length xs `div` 2 - 1) xs
{-# INLINE fromSortedList #-}

insert :: (Ord k, Foldable t, Unfoldable t, Base t ~ TreeNode k v) => k -> v -> t -> TreeNode k v (Either t t)
insert k v t = case project t of
    Leaf -> Branch k v (Left empty) (Left empty)
    Branch k' v' l r -> case k `compare` k' of
        LT -> Branch k' v' (Right l) (Left r)
        _ -> Branch k' v' (Left l) (Right r)
{-# INLINE insert #-}
