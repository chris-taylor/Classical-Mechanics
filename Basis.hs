{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Basis where

import Trio
import Enumerable
import VectorSpace

class (VectorSpace v, Enumerable (Basis v)) => HasBasis v where
    type Basis v

    basisValue :: Basis v -> v
    coord      :: v -> Basis v -> Scalar v

-- Numeric instances

instance HasBasis Int where
    type Basis Int = ()

    basisValue () = 1
    coord n () = n

instance HasBasis Integer where
    type Basis Integer = ()

    basisValue () = 1
    coord n () = n

instance HasBasis Float where
    type Basis Float = ()

    basisValue () = 1
    coord n () = n

instance HasBasis Double where
    type Basis Double = ()

    basisValue () = 1
    coord n () = n

-- Tuple instances

instance (HasBasis u, HasBasis v, Scalar u ~ Scalar v) => HasBasis (u,v) where
    type Basis (u,v) = Either (Basis u) (Basis v)

    basisValue (Left a)  = (basisValue a, zeroV)
    basisValue (Right b) = (zeroV, basisValue b)

    coord (u,v) = either (coord u) (coord v)

instance (HasBasis u, HasBasis v, HasBasis w, Scalar u ~ Scalar v, Scalar v ~ Scalar w) => HasBasis (u,v,w) where
    type Basis (u,v,w) = Trio (Basis u) (Basis v) (Basis w)

    basisValue (First a)  = (basisValue a, zeroV, zeroV)
    basisValue (Second b) = (zeroV, basisValue b, zeroV)
    basisValue (Third c)  = (zeroV, zeroV, basisValue c)

    coord (u,v,w) = trio (coord u) (coord v) (coord w)
