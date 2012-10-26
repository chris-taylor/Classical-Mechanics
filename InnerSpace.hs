module InnerSpace ( InnerSpace(..) ) where

import VectorSpace

-- |This class describes spaces with an inner product.
class (VectorSpace v) => InnerSpace v where
    dot :: v -> v -> Scalar v

-- Primitive instances

instance InnerSpace Int where
    dot = (*)

instance InnerSpace Integer where
    dot = (*)

instance InnerSpace Float where
    dot = (*)

instance InnerSpace Double where
    dot = (*)

-- Tuple instances

