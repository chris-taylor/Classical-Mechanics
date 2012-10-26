{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module Vector2 ( V2(..) ) where

import Control.Applicative hiding ((*>))
import VectorSpace
import InnerSpace

data V2 a = V2 !a !a deriving (Eq,Ord)

instance Show a => Show (V2 a) where
    showsPrec _ (V2 x y) =
        showChar '(' . shows x . showString ", " . shows y . showChar ')'

instance Functor V2 where
    fmap f (V2 x y) = V2 (f x) (f y)

instance Applicative V2 where
    pure a            = V2 a a
    V2 f g <*> V2 x y = V2 (f x) (g y)

instance Num a => Num (V2 a) where
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    signum = fmap signum
    abs    = fmap abs

instance Fractional a => Fractional (V2 a) where
    fromRational = pure . fromRational
    (/)   = liftA2 (/)
    recip = fmap recip

instance Floating a => Floating (V2 a) where
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

instance Num a => AdditiveGroup (V2 a) where
    zeroV   = V2 0 0
    (<+>)   = liftA2 (+)
    negateV = liftA negate

instance Num v => VectorSpace (V2 v) where
    type Scalar (V2 v) = v

    s *> V2 a b = V2 (s * a) (s * b)

    toList (V2 a b) = [a,b]
    fromList [a,b]  = V2 a b

instance Num v => InnerSpace (V2 v) where
    V2 a b `dot` V2 c d = a * c + b * d
