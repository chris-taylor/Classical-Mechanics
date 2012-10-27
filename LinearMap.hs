{-# LANGUAGE TypeOperators, TypeFamilies #-}

--module LinearMap ( (:->), lapply, linear, Basis(..), VectorSpace (..) ) where
module LinearMap ( (:->), lapply, Basis(..), VectorSpace(..) ) where

import Basis
import VectorSpace

type MSum a = Maybe (Sum a)

type LMap' u v = MSum (Basis u -> v)

newtype u :-> v = LMap { unLMap :: LMap' u v }

atZ :: (AdditiveGroup b) => (a -> b) -> (MSum a -> b)
atZ f = maybe zeroV (f . getSum)

jsum :: a -> MSum a
jsum = Just . Sum

-- |Build a linear map from a function on vectors.
linear :: (HasBasis u) => (u -> v) -> (u :-> v)
linear f = LMap (jsum (f . basisValue))

-- |Apply a linear map to a vector.
lapply :: (HasBasis u, Scalar u ~ Scalar v,
           VectorSpace v) => (u :-> v) -> (u -> v)
lapply = atZ lapply' . unLMap

-- |Helper function, useful for lapply.
lapply' :: (HasBasis u, Scalar u ~ Scalar v,
            VectorSpace v) => (Basis u -> v) -> (u -> v)
lapply' m u = linearCombo [ (coord u e, m e) | e <- enumerate ]

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