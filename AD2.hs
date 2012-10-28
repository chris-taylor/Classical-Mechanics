{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module AD2 where

import Control.Applicative hiding ((*>), (<*))
import LinearMap
import VectorSpace
import InnerSpace

------------------------------
-- Numeric Functions
------------------------------

instance Num b => Num (a -> b) where
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
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
-- AD Types
------------------------------

-- |Type encapsulating both value and derivative.
data a :> b = D { value :: b, derivative :: a :-> b }

-- |Type of differentiable functions.
type a :~> b = a -> (a :> b)

showsD :: Show b => (a :> b) -> ShowS
showsD (D val _) = showString "D " . shows val . showString " ..."

--data a :> b = D { value :: b, derivative :: a :-> (a :> b) }

noOp :: String -> a
noOp op = error (op ++ " not defined on a :> b")

--constD :: b -> a :> b
--constD :: (HasBasis a, VectorSpace (a :> b),
--           Scalar a ~ Scalar (a :> b)) => b -> a :> b
constD b = D b zeroV

--fmapD :: (b -> c) -> (a :> b) -> (a :> c)
fmapD :: (AdditiveGroup b) => (b -> c) -> (a :> b) -> (a :> c)
fmapD f (D a0 a') = D (f a0) (fmapL f a')

--linearD :: (HasBasis a, VectorSpace (a :> b),
--            Scalar a ~ Scalar (a :> b)) => (a -> b) -> a -> a :> b
--linearD f = \u -> D (f u) d
--    where
--        d = linear (constD . f)
linearD :: HasBasis a => (a -> b) -> (a :~> b)
linearD f = \u -> D (f u) d
    where
        d = linear f

idD :: (HasBasis a) => (a :~> a)
idD = linearD id

fstD :: (HasBasis a, HasBasis b, Scalar a ~ Scalar b) => (a, b) :~> a
fstD x = linearD fst x

sndD :: (HasBasis a, HasBasis b, Scalar a ~ Scalar b) => (a, b) :~> b
sndD x = linearD snd x

(><) :: (HasBasis a, Scalar a ~ Scalar b,
         HasBasis b, Scalar b ~ Scalar c,
         VectorSpace c)
        => (b -> c) -> (b -> (b :-> c)) -> (a :> b) -> (a :> c)

(g >< dg) (D fx dfx) = D (g fx) (dg fx `compose` dfx)

distrib :: (AdditiveGroup a, AdditiveGroup b,
            HasBasis u, Scalar u ~ Scalar v,
            VectorSpace v) =>
     (a -> b -> v) -> (u :> a) -> (u :> b) -> u :> v

distrib op (D u0 u') (D v0 v') =
    D (u0 `op` v0) ( (fmapL (`op` v0) u') <+> (fmapL (u0 `op`) v') )

deriv :: (HasBasis u, Scalar u ~ Scalar v, VectorSpace v)
      => (u :> v) -> u -> v
deriv = lapply . derivative

derivV :: (HasBasis u, Scalar u ~ Scalar v, VectorSpace v)
       => (u :> v) -> u -> (v, v)
derivV d x = (value d, deriv d x)

diff f x = deriv (f (idD x)) x

diffV f x = derivV (f (idD x)) x

---------- Instances

instance Show b => Show (a :> b) where
    showsPrec _ = showsD

instance Eq b => Eq (a :> b) where
    (==) = noOp "(==)"

instance (HasBasis u, Scalar u ~ Scalar v,
          VectorSpace v) => AdditiveGroup (u :> v) where

    zeroV   = constD zeroV
    negateV = fmapD negateV

    D u0 u' <+> D v0 v' = D (u0 <+> v0) (u' <+> v')
    D u0 u' <-> D v0 v' = D (u0 <-> v0) (u' <-> v')

instance (HasBasis u, VectorSpace v, Scalar u ~ Scalar v,
          AdditiveGroup (Scalar v)) => VectorSpace (u :> v) where

    type Scalar (u :> v) = u :> Scalar v

    (*>) = distrib (*>)

instance (HasBasis u, InnerSpace v, Scalar u ~ Scalar v, VectorSpace (Scalar v),
          AdditiveGroup (Scalar v), Scalar (Scalar v) ~ Scalar v) => InnerSpace (u :> v) where

    -- this requires UndecidableInstances at the moment - how to fix it?
    dot = distrib dot

infix 0 >-< -- necessary for the Num, Fractional and Floating instances

(>-<) :: VectorSpace v => (v -> v) -> (v -> Scalar v) -> (a :> v) -> a :> v
(f >-< df) (D u du) = D (f u) (fmapL (df u *>) du)

instance (HasBasis a, Scalar a ~ Scalar v,
          VectorSpace v, v ~ Scalar v, Num v) => Num (a :> v) where
    fromInteger = constD . fromInteger

    (+) = (<+>)
    (-) = (<->)
    (*) = distrib (*)

    signum = signum >-< abs
    abs    = abs    >-< const 0

sqr x = x * x

instance (HasBasis a, Scalar a ~ Scalar v,
          VectorSpace v, Fractional v, v ~ Scalar v) => Fractional (a :> v) where

    fromRational = constD . fromRational

    recip = recip >-< - recip sqr

instance (HasBasis a, Floating v, VectorSpace v,
          Scalar a ~ Scalar v, Scalar v ~ v) => Floating (a :> v) where

    pi    = constD pi

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


