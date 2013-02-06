module Control.Functor.Algebra (
      Algebra
    , Coalgebra
    ) where

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a
