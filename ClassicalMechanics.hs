-- {-# LANGUAGE FlexibleInstances #-}

module ClassicalMechanics where

import Expr
import Integration
import Control.Applicative
import Prelude hiding (Real,(^))

------------------------------
-- Vector type
------------------------------

data Vector a = V !a !a !a deriving (Eq)

instance Show a => Show (Vector a) where
    show (V x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

instance Functor Vector where
    fmap f (V x y z) = V (f x) (f y) (f z)

instance Applicative Vector where
    pure a = V a a a
    V f g h <*> V x y z = V (f x) (g y) (h z)

dot :: Num a => Vector a -> Vector a -> a
dot (V x y z) (V x' y' z') = x * x' + y * y' + z * z'

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
    (/) = liftA2 (/)

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
-- Local coordinates
------------------------------

data Local f a = Local !a !(f a) !(f a) deriving (Eq,Show)

time :: Local f a -> a
time (Local t _ _) = t

position :: Local f a -> f a
position (Local _ pos _) = pos

velocity :: Local f a -> f a
velocity (Local _ _ vel) = vel

coord = Local t (V x y z) (V x' y' z')

-- |Lift any functor of functions to a function that acts to produce a functor.
up :: Functor f => f (a -> b) -> a -> f b
up fs t = fmap ($t) fs

-- |Coordinate function.
q :: Expr -> Vector Expr
q = up $ V (literalFunction x)
           (literalFunction y)
           (literalFunction z)

-- |Create a literal symbolic function.
literalFunction :: Expr -> Expr -> Expr
literalFunction (Var f) x = Lit f x

------------------------------
-- Automatic differentiation
------------------------------

data Dif a = D a (Dif a) deriving (Show)

dConst :: Num a => a -> Dif a
dConst x = D x dZero

dZero :: Num a => Dif a
dZero = D 0 dZero

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
    recip        = dlift recip (sqr recip)

instance Floating a => Floating (Dif a) where
    pi    = dConst pi
    exp   = exp   >-< exp
    log   = log   >-< recip
    sqrt  = sqrt  >-< recip (2 * sqrt)
    sin   = sin   >-< cos
    cos   = cos   >-< negate . sin
    sinh  = sinh  >-< cosh
    cosh  = cosh  >-< sinh
    asin  = asin  >-< recip (sqrt (1-sqr))
    acos  = acos  >-< recip (- sqrt (1-sqr))
    atan  = atan  >-< recip (1 + sqr)
    asinh = asinh >-< recip (sqrt (1+sqr))
    acosh = acosh >-< recip (- sqrt (sqr-1))
    atanh = atanh >-< recip (1-sqr)


------------------------------
-- Symbolic derivatives
------------------------------

class Fractional a => Differentiable a where
    d :: Applicative f => (a -> f a) -> a -> f a

instance Differentiable Float where
    d f x = (/(2*dx)) <$> (liftA2 (-) (f (x+dx)) (f (x-dx)))
        where
            dx  = 1e-8

instance Differentiable Expr where
    d f = fmap (Lit "D") . f

--d :: Functor f => (Expr -> f Expr) -> Expr -> f Expr
--d q = fmap (Lit "D") . q

------------------------------
-- Local coordinate function
------------------------------

gamma :: (Applicative f, Differentiable a) => (a -> f a) -> a -> Local f a
gamma q t = Local t (q t) (d q t)

------------------------------
-- Free particle
------------------------------

type Mass = Real

lagrangianFreeParticle :: Fractional a => a -> Local Vector a -> a
lagrangianFreeParticle mass local = 0.5 * mass * (dot v v)
    where v = velocity local

------------------------------
-- Lagrangian action function
------------------------------

lagrangianAction :: (Applicative f, Differentiable a) => (Local f a -> a) -> (a -> f a) -> a -> a -> a
lagrangianAction l q t1 t2 = definiteIntegral (l . gamma q) t1 t2

-- Test path

testPath t = V (4 * t + 7) (3 * t + 5) (2 * t + 1)
