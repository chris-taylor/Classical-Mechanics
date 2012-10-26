{-# LANGUAGE TypeOperators, TypeFamilies #-}

module LinearMap where

import Basis
import VectorSpace

type u :-> v = Basis u -> v

-- |Apply a linear map to a vector.
lapply :: (HasBasis u, VectorSpace v, Scalar u ~ Scalar v) => (u :-> v) -> (u -> v)
lapply f u = sumV [ coord u e *> f e | e <- enumerate ]

-- |Build a linear map from a function on vectors.
linear :: (HasBasis u) => (u -> v) -> (u :-> v)
linear f = f . basisValue