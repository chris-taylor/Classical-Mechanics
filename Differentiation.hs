module Differentiation where

import Control.Applicative

------------------------------
-- Numeric Applicatives
------------------------------

instance Num b => Num (a -> b) where
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (-)
    negate = fmap negate
    signum = fmap signum
    abs    = fmap abs

instance Fractional b => Fractional (a -> b) where
    fromRational = pure . fromRational
    (/)   = liftA2 (/)
    recip = fmap recip

instance Floating b => Floating (a -> b) where
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

------------------------------
-- Automatic differentiation
------------------------------

data Dif a = D a (Dif a) deriving (Show)

dConst :: Num a => a -> Dif a
dConst x = D x dZero

dZero :: Num a => Dif a
dZero = D 0 dZero

dVar :: Num a => a -> Dif a
dVar x = D x (dConst 1)

dlift :: Num a => (a -> a) -> (Dif a -> Dif a) -> Dif a -> Dif a
dlift f f' = \ u@(D u0 u') -> D (f u0) (f' u * u')

infix 0 >-<
(>-<) :: Num a => (a -> a) -> (Dif a -> Dif a) -> Dif a -> Dif a
(>-<) = dlift

sqr :: Num a => a -> a
sqr x = x * x

instance Functor Dif where
    fmap f (D a b) = D (f a) (fmap f b)

instance Num a => Num (Dif a) where
    fromInteger               = dConst . fromInteger
    D x0 x' + D y0 y'         = D (x0 + y0) (x' + y')
    D x0 x' - D y0 y'         = D (x0 - y0) (x' - y')
    x@(D x0 x') * y@(D y0 y') = D (x0 * y0) (x' * y + x * y')

    negate = negate >-< (-1)
    abs    = abs    >-< signum
    signum = signum >-< 0

instance Fractional a => Fractional (Dif a) where
    fromRational = dConst . fromRational
    recip        = recip >-< -(sqr recip)

instance Floating a => Floating (Dif a) where
    pi    = dConst pi
    exp   = exp   >-< exp
    log   = log   >-< recip
    sqrt  = sqrt  >-< recip (2 * sqrt)
    sin   = sin   >-< cos
    cos   = cos   >-< -sin
    sinh  = sinh  >-< cosh
    cosh  = cosh  >-< sinh
    asin  = asin  >-< recip (sqrt (1-sqr))
    acos  = acos  >-< recip (- sqrt (1-sqr))
    atan  = atan  >-< recip (1 + sqr)
    asinh = asinh >-< recip (sqrt (1+sqr))
    acosh = acosh >-< recip (- sqrt (sqr-1))
    atanh = atanh >-< recip (1-sqr)

dTake :: Int -> Dif a -> [a]
dTake 0 _         = []
dTake n (D a dif) = a : dTake (n-1) dif
