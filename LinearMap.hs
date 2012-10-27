{-# LANGUAGE TypeOperators, TypeFamilies, NoMonomorphismRestriction #-}

module LinearMap ( (:->), lapply, linear, Basis(..), VectorSpace (..) ) where

import Basis
import VectorSpace

newtype u :-> v = LMap { unLMap :: Basis u -> v }

-- |Apply a linear map to a vector.
lapply :: (HasBasis u, VectorSpace v, Scalar u ~ Scalar v) => (u :-> v) -> (u -> v)
lapply f u = sumV [ coord u e *> unLMap f e | e <- enumerate ]

-- |Build a linear map from a function on vectors.
linear :: (HasBasis u) => (u -> v) -> (u :-> v)
linear f = LMap (f . basisValue)

-- |Identity map.
idL :: (HasBasis v) => v :-> v
idL = linear id

-- |Composition of linear maps.
compose :: (HasBasis a, Scalar a ~ Scalar b,
            HasBasis b, Scalar b ~ Scalar c,
            VectorSpace c) =>
           (b :-> c)
        -> (a :-> b)
        -> (a :-> c)
compose f g = linear (lapply f . lapply g)