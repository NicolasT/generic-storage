{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, KindSignatures #-}

module Data.Tree.Annotated where

import Prelude hiding (mapM)
import Data.Ord (comparing)
import Data.List (sortBy)
import Control.Functor.Algebra
import Control.Monad ((<=<))
import Data.Traversable (Traversable, mapM)

import qualified Data.Tree.Algorithms as A

type Tree a k v = FixA a (A.TreeNode k v)

newtype FixA (a :: (* -> *) -> (* -> *)) (f :: (* -> *)) = FixA (a f (FixA a f))

unFixA :: FixA a f -> a f (FixA a f)
unFixA (FixA a) = a

class (Monad m) => Out a f m where
  outA :: FixA a f -> m (f (FixA a f))

class (Monad m) => In a f m where
  inA :: f (FixA a f) -> m (FixA a f)

cataA :: (Traversable f, Out a f m) => Algebra f b -> FixA a f -> m b
cataA phi = return . phi <=< mapM (cataA phi) <=< outA

anaA :: (Traversable f, In a f m) => Coalgebra f b -> b -> m (FixA a f)
anaA psi = inA <=< mapM (anaA psi) <=< return . psi

lookup :: (Ord k, Out a (A.TreeNode k v) m) => k -> Tree a k v -> m (Maybe v)
lookup k = cataA (A.lookup k)

fromSortedList :: (In a (A.TreeNode k v) m) => [(k, v)] -> m (Tree a k v)
fromSortedList = anaA A.fromSortedList

fromList :: (Ord k, In a (A.TreeNode k v) m) => [(k, v)] -> m (Tree a k v)
fromList = fromSortedList . sortBy (comparing fst)
