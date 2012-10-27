{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module InnerSpace ( InnerSpace(..), (<.>), magnitudeSq, magnitude, normalized ) where

import VectorSpace

-- |This class describes spaces with an inner product.
class (VectorSpace v) => InnerSpace v where
    dot :: v -> v -> Scalar v

(<.>) :: (InnerSpace v) => v -> v -> Scalar v
(<.>) = dot

magnitudeSq :: (InnerSpace v) => v -> Scalar v
magnitudeSq v = dot v v

magnitude :: (InnerSpace v, Floating (Scalar v)) => v -> Scalar v
magnitude = sqrt . magnitudeSq

normalized :: (InnerSpace v, Floating (Scalar v)) => v -> v
normalized v = v </ magnitude v

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

instance (InnerSpace u, InnerSpace v, Scalar u ~ Scalar v,
          AdditiveGroup (Scalar u)) => InnerSpace (u,v) where
    dot (a,b) (c,d) = dot a c <+> dot b d

instance (InnerSpace u, Scalar u ~ Scalar v,
          InnerSpace v, Scalar v ~ Scalar w,
          InnerSpace w, AdditiveGroup (Scalar w)) => InnerSpace (u,v,w) where
    dot (a,b,c) (d,e,f) = dot a d <+> dot b e <+> dot c f