{-# LANGUAGE TypeFamilies #-}

module VectorSpace where

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

-- | Division by a scalar.
(</) :: (VectorSpace v, s ~ Scalar v, Fractional s) => v -> s -> v
v </ s = (1/s) *> v

-- |This class describes spaces with an inner product.
class VectorSpace v => InnerSpace v where
    dot :: v -> v -> Scalar v

------------------------------
-- VectorSpace Instances
------------------------------

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

instance VectorSpace b => VectorSpace (a -> b) where
    type Scalar (a -> b) = Scalar b
    s *> v  = \a -> s *> v a

------------------------------
-- InnerSpace Instances
------------------------------

instance InnerSpace Int where
    dot = (*)

instance InnerSpace Integer where
    dot = (*)

instance InnerSpace Float where
    dot = (*)

instance InnerSpace Double where
    dot = (*)
