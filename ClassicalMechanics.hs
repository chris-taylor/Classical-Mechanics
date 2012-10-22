-- {-# LANGUAGE FlexibleInstances #-}

module ClassicalMechanics where

import Control.Applicative
import Prelude hiding (Real)

import Expr
import Differentiation
import Integration
import Optimization

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

instance Num a => Num (Vector a) where
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    signum = fmap signum
    abs    = fmap abs

instance Fractional a => Fractional (Vector a) where
    fromRational = pure . fromRational
    (/)   = liftA2 (/)
    recip = fmap recip

instance Floating a => Floating (Vector a) where
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

dot :: Num a => Vector a -> Vector a -> a
dot (V x y z) (V x' y' z') = x * x' + y * y' + z * z'

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
literalFunction f expr = atomE $ App (getVar f) expr

------------------------------
-- Symbolic derivatives
------------------------------

class Fractional a => Differentiable a where
    d :: Applicative f => (a -> f a) -> a -> f a

instance Differentiable Float where
    d f x = fmap (/(2*dx)) $ (liftA2 (-) (f (x+dx)) (f (x-dx)))
        where
            dx  = 1e-4

instance Differentiable Double where
    d f x = fmap (/(2*dx)) $ (liftA2 (-) (f (x+dx)) (f (x-dx)))
        where
            dx  = 1e-8

instance Differentiable Expr where
    d f = fmap (literalFunction (varE "D")) . f

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

lagrangianAction :: (Applicative f) => (Local f Real -> Real) -> (Real -> f Real) -> Real -> Real -> Real
lagrangianAction l q t1 t2 = definiteIntegral (l . gamma q) t1 t2

------------------------------
-- Action over a test path
------------------------------

testPath :: Real -> Vector Real
testPath t = V (4 * t + 7) (3 * t + 5) (2 * t + 1)

makeEta :: (Applicative f, Num (f Real)) =>
           (Real -> f Real)
        -> Real
        -> Real
        -> Real -> f Real
makeEta nu t1 t2 t = pure (t - t1) * pure (t - t2) * nu t

variedFreeParticleAction :: Real
                         -> (Real -> Vector Real)
                         -> (Real -> Vector Real)
                         -> Real
                         -> Real
                         -> Real
                         -> Real 
variedFreeParticleAction mass q nu t1 t2 epsilon =
    let eta = makeEta nu t1 t2
     in lagrangianAction (lagrangianFreeParticle mass) (q + (fmap.fmap) (epsilon*) eta) t1 t2
