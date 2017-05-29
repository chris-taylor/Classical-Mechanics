{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module HCMUtils.Basis
    ( decompose'
    , recompose'
    , recompose
    , toCoords
    , fromCoords
    , dim
    , HasBasis(..)
    , Enumerable(..)
    ) where

import HCMUtils.Trio
import HCMUtils.Enumerable
import HCMUtils.VectorSpace

class (VectorSpace v, Enumerable (Basis v)) => HasBasis v where
    type Basis v

    basisValue :: Basis v -> v
    decompose  :: v -> Basis v -> Scalar v

decompose' :: (HasBasis v) => v -> [(Basis v, Scalar v)]
decompose' v = [ (e, decompose v e) | e <- enumerate ]

recompose :: (HasBasis v) => (Basis v -> Scalar v) -> v
recompose f = recompose' [ (e, f e) | e <- enumerate ]

recompose' :: (HasBasis v) => [(Basis v,Scalar v)] -> v
recompose' ps = sumV [ s *> basisValue e | (e, s) <- ps ]

toCoords :: (HasBasis v) => v -> [Scalar v]
toCoords v = map snd (decompose' v)

fromCoords :: (HasBasis v) => [Scalar v] -> v
fromCoords vs = recompose' (zip enumerate vs)

dim :: (HasBasis v) => v -> Int
dim v = length (toCoords v)

-- Numeric instances

instance HasBasis Int where
    type Basis Int = ()
    basisValue ()  = 1
    decompose s () = s

instance HasBasis Integer where
    type Basis Integer = ()
    basisValue ()      = 1
    decompose s ()     = s

instance HasBasis Float where
    type Basis Float = ()
    basisValue ()    = 1
    decompose s ()   = s

instance HasBasis Double where
    type Basis Double = ()
    basisValue ()     = 1
    decompose s ()    = s

-- Tuple instances

instance (HasBasis u, HasBasis v, Scalar u ~ Scalar v) => HasBasis (u,v) where
    type Basis (u,v) = Either (Basis u) (Basis v)

    basisValue (Left a)  = (basisValue a, zeroV)
    basisValue (Right b) = (zeroV, basisValue b)

    decompose (u,v) = either (decompose u) (decompose v)

instance (HasBasis u, HasBasis v, HasBasis w, Scalar u ~ Scalar v, Scalar v ~ Scalar w) => HasBasis (u,v,w) where
    type Basis (u,v,w) = Trio (Basis u) (Basis v) (Basis w)

    basisValue (First a)  = (basisValue a, zeroV, zeroV)
    basisValue (Second b) = (zeroV, basisValue b, zeroV)
    basisValue (Third c)  = (zeroV, zeroV, basisValue c)

    decompose (u,v,w) = trio (decompose u) (decompose v) (decompose w)
