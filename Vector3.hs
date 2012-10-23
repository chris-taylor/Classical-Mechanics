{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module Vector3 where

import Control.Applicative hiding ((*>))
import AdditiveGroup
import VectorSpace

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

instance AdditiveGroup v => AdditiveGroup (V3 v) where
    zeroV   = V3 zeroV zeroV zeroV
    (<+>)   = liftA2 (<+>)
    negateV = liftA negateV

instance VectorSpace v => VectorSpace (V3 v) where
    type Scalar (V3 v) = Scalar v
    s *> V3 a b c = V3 (s *> a) (s *> b) (s *> c)

instance (InnerSpace v, AdditiveGroup (Scalar v)) => InnerSpace (V3 v) where
    V3 a b c `dot` V3 d e f = a `dot` d <+> b `dot` e <+> c `dot` f
