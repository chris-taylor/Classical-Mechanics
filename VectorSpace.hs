{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module VectorSpace
    ( module AdditiveGroup
    , VectorSpace(..)
    , (</)
    , lerp
    , linearCombo ) where

import AdditiveGroup

infixl 7 *>
infixr 7 </

-- |This class describes vector spaces. A vector space V over a field F is a pair (V,F) where V is
--an additive group, and there is an operation (A x V) -> V which is linear in the sense that
--
--    a *> (u <+> v) === a *> u <+> a *> v
--
--for all a, u and v.
class AdditiveGroup v => VectorSpace v where
    type Scalar v

    -- | Multiplication by a scalar on the left.
    (*>)  :: Scalar v -> v -> v 

-- |Division by a scalar.
(</) :: (VectorSpace v, s ~ Scalar v, Fractional s) => v -> s -> v
v </ s = (1/s) *> v

-- |Linear interpolation between two vectors.
lerp :: (VectorSpace v) => v -> v -> Scalar v -> v
lerp u v t = u <+> t *> (v <-> u)

-- |Linear combination of vectors.
linearCombo :: (VectorSpace v) => [(Scalar v,v)] -> v
linearCombo ps = sumV [ s *> v | (s,v) <- ps ]

-- Primitive instances

instance VectorSpace Int where
    type Scalar Int = Int
    (*>) = (*)

instance VectorSpace Integer where
    type Scalar Integer = Integer
    (*>) = (*)

instance VectorSpace Float where
    type Scalar Float = Float
    (*>) = (*)

instance VectorSpace Double where
    type Scalar Double = Double
    (*>) = (*)

-- Function instance

instance VectorSpace b => VectorSpace (a -> b) where
    type Scalar (a -> b) = Scalar b
    s *> v   = \a -> s *> v a

-- Tuple instances

instance (VectorSpace u, VectorSpace v, Scalar u ~ Scalar v) => VectorSpace (u,v) where
    type Scalar (u,v) = Scalar u

    s *> (u,v) = (s *> u, s *> v)

instance (VectorSpace u, VectorSpace v, VectorSpace w, Scalar u ~ Scalar v, Scalar v ~ Scalar w) => VectorSpace (u,v,w) where
    type Scalar (u,v,w) = Scalar u

    s *> (u,v,w) = (s *> u, s *> v, s *> w)


