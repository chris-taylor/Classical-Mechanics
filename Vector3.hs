{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module Vector3 ( V3(..) ) where

import Control.Applicative hiding ((*>))

import Basis
import VectorSpace
import InnerSpace

data V3 a = V3 !a !a !a deriving (Eq,Ord)

instance Show a => Show (V3 a) where
    showsPrec _ (V3 x y z) =
        showChar '(' . shows x . showString ", " . shows y . showString ", " . shows z . showChar ')'

instance Functor V3 where
    fmap f (V3 x y z) = V3 (f x) (f y) (f z)

instance Applicative V3 where
    pure a                = V3 a a a
    V3 f g h <*> V3 x y z = V3 (f x) (g y) (h z)

instance Num a => Num (V3 a) where
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    signum = fmap signum
    abs    = fmap abs

instance Fractional a => Fractional (V3 a) where
    fromRational = pure . fromRational
    (/)   = liftA2 (/)
    recip = fmap recip

instance Floating a => Floating (V3 a) where
    pi    = pure pi
    exp   = liftA exp
    log   = liftA log
    sqrt  = liftA sqrt
    sin   = liftA sin
    cos   = liftA cos
    asin  = liftA asin
    acos  = liftA acos
    atan  = liftA atan
    sinh  = liftA sinh
    cosh  = liftA cosh
    asinh = liftA asinh
    acosh = liftA acosh
    atanh = liftA atanh

instance Num a => AdditiveGroup (V3 a) where
    zeroV   = V3 0 0 0
    (<+>)   = liftA2 (+)
    negateV = liftA negate

instance Num a => VectorSpace (V3 a) where
    type Scalar (V3 a) = a

    s *> V3 a b c = V3 (s * a) (s * b) (s * c)

instance Num a => InnerSpace (V3 a) where
    V3 a b c `dot` V3 a' b' c' = a * a' + b * b' + c * c'

instance Num a => Enumerable (V3 a) where
    enumerate = [ V3 1 0 0, V3 0 1 0, V3 0 0 1 ]

instance Num a => HasBasis (V3 a) where
    type Basis (V3 a) = V3 a

    basisValue = id
    decompose  = dot
