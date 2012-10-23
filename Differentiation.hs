{-# LANGUAGE TypeFamilies #-}

module Differentiation where

import VectorSpace

-- |Class for differentiable types.
class Differentiable a where
    d :: (VectorSpace v, a ~ Scalar v) => (a -> v) -> a -> v

instance Differentiable Float where
    d f x = ( f(x+dx) <-> f(x-dx) ) </ (2 * dx)
        where dx = 1e-4

instance Differentiable Double where
    d f x = ( f(x+dx) <-> f(x-dx) ) </ (2 * dx)
        where dx  = 1e-8
