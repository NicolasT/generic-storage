{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, FlexibleInstances, FlexibleContexts #-}

module Main where

import Prelude hiding (lookup)
import Data.Tree.Annotated
import Control.Monad

import qualified Data.Tree.Algorithms as A

newtype Debug f a = Debug { unDebug :: f a }
  deriving (Functor)

instance (Functor f, Show (f ())) => In Debug f IO where
    inA = return . FixA . Debug <=< printer "in"

instance (Functor f, Show (f ())) => Out Debug f IO where
    outA = printer "out" . unDebug . unFixA

printer :: (Functor f, Show (f ())) => String -> f a -> IO (f a)
printer s f = print (s, void f) >> return f

t :: (In a (A.TreeNode String Int) m) => m (FixA a (A.TreeNode String Int))
t = fromList [("a", 1), ("b", 2), ("c", 3)]

main :: IO ()
main = do
    putStrLn "Rendering tree"
    t' <- t :: IO (FixA Debug (A.TreeNode String Int))
    putStrLn "Looking up a node"
    n <- lookup "a" t'
    putStrLn $ "Result: " ++ show n
