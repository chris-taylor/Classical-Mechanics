{-# LANGUAGE TypeOperators, TypeFamilies #-}

module LinearMap
    ( (:->)
    , linear
    , lapply
    , idL
    , compose
    , inLMap
    , liftL
    , liftL2
    , fmapL
    , fmapL2
    , HasBasis (..)
    , VectorSpace (..)
    ) where

import Control.Applicative hiding ((*>), (<*))

import Iso
import Basis
import VectorSpace

type MSum a = Maybe (Sum a)

type LMap' u v = MSum (Basis u -> v)

newtype u :-> v = LMap { unLMap :: LMap' u v }

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
lapply' m u = linearCombo [ (decompose u e, m e) | e <- enumerate ]

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

---------- Linear maps are vector spaces?

instance (HasBasis u, VectorSpace v,
          Scalar u ~ Scalar v) => AdditiveGroup (u :-> v) where

    zeroV = LMap Nothing

    LMap Nothing <+> b            = b
    a            <+> LMap Nothing = a
    a            <+> b            = linear (\u -> lapply a u <+> lapply b u)

instance (HasBasis u, VectorSpace v,
          Scalar u ~ Scalar v) => VectorSpace (u :-> v) where
    
    type Scalar (u :-> v) = Scalar v

    s *> LMap Nothing = LMap Nothing
    s *> a            = linear (\u -> s *> lapply a u)

---------- Convenience functions

atZ :: (AdditiveGroup b) => (a -> b) -> (MSum a -> b)
atZ f = maybe zeroV (f . getSum)

jsum :: a -> MSum a
jsum = Just . Sum

inLMap :: (LMap' a b -> LMap' a' b') -> (a :-> b) -> (a' :-> b')
inLMap = unLMap ~> LMap

inLMap2 :: (LMap' a b -> LMap' a' b' -> LMap' a'' b'')
        -> (a :-> b) -> (a' :-> b') -> (a'' :-> b'')
inLMap2 = unLMap ~> inLMap

---------- Lift / Map etc for MSum

-- |Lift a function over the 'MSum' type.
liftMS :: (a -> b) -> MSum a -> MSum b
liftMS = fmap . fmap

-- |Lift a binary function over the 'MSum' type.
liftMS2 :: (AdditiveGroup a, AdditiveGroup b) => (a -> b -> c) -> MSum a -> MSum b -> MSum c
liftMS2 f ma mb = jsum $ f (fromMS ma) (fromMS mb) -- doesn't optimize zeros!

-- |Get a vector out of an 'MSum'.
fromMS :: AdditiveGroup v => MSum v -> v
fromMS Nothing        = zeroV
fromMS (Just (Sum u)) = u

---------- Lift / Map etc for Linear maps

-- |Lift a linear function to each element of a linear map.
liftL :: (Functor f, AdditiveGroup (f a)) =>
         (a -> b) -> MSum (f a) -> MSum (f b)
liftL = liftMS . fmap

-- |Lift a binary linear function over a linear map.
liftL2 :: (Applicative f, AdditiveGroup (f a), AdditiveGroup (f b)) =>
          (a -> b -> c) -> MSum (f a) -> MSum (f b) -> MSum (f c)
liftL2 = liftMS2 . liftA2

-- |Map a function over a linear map.
fmapL :: (AdditiveGroup b) =>
         (b -> c) -> (a :-> b) -> (a :-> c)
fmapL = inLMap . liftL

-- |Map a binary function over a linear map.
fmapL2 :: (AdditiveGroup b, AdditiveGroup c) =>
          (b -> c -> d) -> (a :-> b) -> (a :-> c) -> a :-> d
fmapL2 = inLMap2 . liftL2
