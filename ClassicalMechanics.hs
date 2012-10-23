{-# LANGUAGE TypeFamilies #-}

module ClassicalMechanics where

import Control.Applicative hiding ((*>), (<*))
import Prelude hiding (Real)

import Expr
import AD
import Integration
import Optimization

import VectorSpace
import Vector2
import Vector3

------------------------------
-- Local coordinates
------------------------------

data Local v a = Local !a !v !v deriving (Eq,Show)

time :: Local v a -> a
time (Local t _ _) = t

position :: Local v a -> v
position (Local _ pos _) = pos

velocity :: Local v a -> v
velocity (Local _ _ vel) = vel

--coord = Local t (V3 x y z) (V3 x' y' z')

-- |Lift any functor of functions to a function that acts to produce a functor.
up :: Functor f => f (a -> b) -> a -> f b
up fs t = fmap ($t) fs

-- |Coordinate function.
--q :: Expr -> V3 Expr
--q = up $ V3 (literalFunction x)
--            (literalFunction y)
--            (literalFunction z)

------------------------------
-- Symbolic derivatives
------------------------------

class Differentiable a where
    d :: (VectorSpace v, a ~ Scalar v) => (a -> v) -> a -> v

instance Differentiable Double where
    d f x = ( f(x+dx) <-> f(x-dx) ) </ (2 * dx)
        where dx  = 1e-8

--instance Differentiable Expr where
--    d f x = fmap diffExpr $ f x
--            where diffExpr = atomE . modifyA ('D':) . getAtom

------------------------------
-- Local coordinate function
------------------------------

gamma :: (VectorSpace v, s ~ Scalar v, Differentiable s) => (s -> v) -> s -> Local v s
gamma q t = Local t (q t) (d q t)

------------------------------
-- Free particle
------------------------------

-- |Free particle Lagrangian. The user should supply a mass and a 'local tuple' consisting of time,
--position and velocity.
lFreeParticle :: (InnerSpace v, s ~ Scalar v, Fractional s) => s -> Local v a -> s
lFreeParticle mass local = 0.5 * mass * (dot v v)
    where v = velocity local

-- |Harmonic oscillator lagrandian. The user should supply a mass and a stiffness constant.
lHarmonic :: (InnerSpace v, s ~ Scalar v, Fractional s) => s -> s -> Local v a -> s
lHarmonic mass k local = 0.5 * mass * (dot v v) + 0.5 * k * (dot q q)
    where q = position local
          v = velocity local

------------------------------
-- Lagrangian action function
------------------------------

lagrangianAction :: (VectorSpace v, Scalar v ~ Real) =>
                    (Local v Real -> Real)  -- lagrangian
                 -> (Real -> v)             -- particle path
                 -> Real                    -- initial time
                 -> Real                    -- final time
                 -> Real                    -- action
lagrangianAction l q t1 t2 = definiteIntegral (l . gamma q) t1 t2

------------------------------
-- Action over a test path
------------------------------

---- |A straight-line path between two points.
testPath :: Real -> V3 Real
testPath t = V3 (4 * t + 7) (3 * t + 5) (2 * t + 1)

---- |Construct a variational path, i.e. one which is zero at the start and end points and nonzero
----in-between.
makeEta :: (VectorSpace v, s ~ Scalar v, Num s) => (s -> v) -> s -> s -> s -> v
makeEta nu t1 t2 t = ( (t - t1) * (t - t2) ) *> nu t

---- |Compute the action of a free particle integrated over a variational path. The first path
----supplied is the 'base' path and the second is the variation.
variedFreeParticleAction :: (InnerSpace v, Scalar v ~ Double) =>
                            Real          -- particle mass
                         -> (Real -> v)   -- path through configuration space
                         -> (Real -> v)   -- function to build deviation
                         -> Real          -- initial time
                         -> Real          -- final time
                         -> Real          -- size of deviation
                         -> Real          -- action
variedFreeParticleAction mass q nu t1 t2 epsilon =
    lagrangianAction (lFreeParticle mass) (q <+> epsilon *> eta) t1 t2
    where
        eta = makeEta nu t1 t2

--parametricPathAction l t0 t1 qs = lagrangianAction l path t0 t1
--    where
--        path = makePath t0 t1 qs

makePath :: (VectorSpace v, s ~ Scalar v, Fractional s, Ord s) => s -> s -> [v] -> (s -> v)
makePath t0 t1 qs =
    let n  = length qs
        ts = linSpace t0 t1 n
     in linearInterpolation ts qs

linSpace :: (Fractional s) => s -> s -> Int -> [s]
linSpace lo hi n = map (\n -> lo + fromIntegral n * dv) [0 .. n-1]
    where
        nr = fromIntegral (n - 1) 
        dv = (1/nr) * (hi - lo)

linSpaceV :: (VectorSpace v, s ~ Scalar v, Fractional s) => v -> v -> Int -> [v]
linSpaceV lo hi n = map (\n -> lo <+> fromIntegral n *> dv) [0 .. n-1]
    where
        nr = fromIntegral (n - 1) 
        dv = (1/nr) *> (hi <-> lo)

--lagrangeInterpolation :: (Eq a, Fractional a) => [a] -> [a] -> a -> a
--lagrangeInterpolation xs ys x = sum $ zipWith (*) ys (map f xs)
--    where
--        f xj    = product $ map (p xj) xs

--        p xj xm = if xj == xm then 1 else (x - xm) / (xj - xm)

linearInterpolation :: (VectorSpace v, s ~ Scalar v, Fractional s, Ord s) => [s] -> [v] -> s -> v
linearInterpolation xs vs x = go (head xs) (head vs) (tail xs) (tail vs)
    where
        go x0 v0 (x1:xs) (v1:vs)
            | x < x1 || null xs = linearInterp1 x0 v0 x1 v1 x
            | otherwise         = go x1 v1 xs vs

linearInterp1 :: (VectorSpace v, s ~ Scalar v, Fractional s) => s -> v -> s -> v -> s -> v
linearInterp1 t0 v0 t1 v1 t = c0 *> v0 <+> c1 *> v1
    where
        c0 = (t1 - t) / dt
        c1 = (t - t0) / dt
        dt = t1 - t0