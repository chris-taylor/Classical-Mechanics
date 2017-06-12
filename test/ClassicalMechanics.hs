{-# LANGUAGE OverloadedStrings, TypeFamilies, TypeOperators,
             FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}

module Main where

import Prelude hiding ((*>), Real)
import Control.Applicative hiding ((*>), (<*))

import HCMUtils.Expr
import HCMUtils.AD2
import HCMUtils.Differentiation
import HCMUtils.Integration
import HCMUtils.Optimization

import HCMUtils.Basis
import HCMUtils.VectorSpace
import HCMUtils.InnerSpace
import HCMUtils.Vector2
import HCMUtils.Vector3

------------------------------
-- Atomic variables
------------------------------

k, m, t, x, y, z, x', y', z' :: Expr

k = "k"
m = "m"
t = "t"

x = "x"
y = "y"
z = "z"

x' = "dx"
y' = "dy"
z' = "dz"

------------------------------
-- Local coordinates
------------------------------

data Local v = Local !(Scalar v) !v !v

deriving instance (Eq v, Eq (Scalar v))     => Eq (Local v)
deriving instance (Ord v, Ord (Scalar v))   => Ord (Local v)
deriving instance (Show v, Show (Scalar v)) => Show (Local v)

time :: Local v -> Scalar v
time (Local t _ _) = t

position :: Local v -> v
position (Local _ pos _) = pos

velocity :: Local v -> v
velocity (Local _ _ vel) = vel

-- this requires UndecidableInstances - can I get rid of that?
--instance (Show v, Show (Scalar v)) => Show (Local v) where
--    showsPrec _ (Local t x v) = showString "Local " . shows t . showString " " .
--                                shows x . showString " " . shows v

--coord = Local t (V3 x y z) (V3 x' y' z')

-- |Lift any functor of functions to a function that acts to produce a functor.
up :: Functor f => f (a -> b) -> a -> f b
up fs t = fmap ($t) fs

-- |Coordinate function.
q :: Expr -> V3 Expr
q = up $ V3 (literalFunction x)
            (literalFunction y)
            (literalFunction z)

------------------------------
-- Local coordinate function
------------------------------

gamma :: (HasBasis v, Differentiable (Scalar v)) => (Scalar v -> v) -> Scalar v -> Local v
gamma q t = Local t (q t) (d q t)

------------------------------
-- Free particle lagrangian
------------------------------

-- |Free particle Lagrangian. The user should supply a mass and a 'local tuple' consisting of time,
--position and velocity.
lFreeParticle :: (InnerSpace v, Fractional (Scalar v)) => Scalar v -> Local v -> Scalar v
lFreeParticle mass local = 0.5 * mass * (dot v v)
    where v = velocity local

------------------------------
-- Harmonic oscillator lagrangian
------------------------------

-- |Harmonic oscillator lagrandian. The user should supply a mass and a stiffness constant.
lHarmonic :: (InnerSpace v, Fractional (Scalar v)) => Scalar v -> Scalar v -> Local v -> Scalar v
lHarmonic mass k local = 0.5 * mass * (dot v v) - 0.5 * k * (dot q q)
    where q = position local
          v = velocity local

------------------------------
-- Lagrangian action function
------------------------------

lagrangianAction :: (HasBasis v, Scalar v ~ Real) =>
                    (Local v -> Real)       -- lagrangian
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
variedFreeParticleAction :: (HasBasis v, InnerSpace v, Scalar v ~ Double) =>
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

-- |Find an approximate solution path through configuration space by numerically minimizing the
--action over a space of parametric paths with fixed initial and final points.
findPath :: (HasBasis v, Scalar v ~ Real) =>
            (Local v -> Real)       -- lagrangian
         -> Real                    -- initial time
         -> v                       -- initial position
         -> Real                    -- final time
         -> v                       -- final position
         -> Int                     -- no. of points on path
         -> (Real -> v)             -- final path
findPath lagrangian t0 q0 t1 q1 n =
    let initialqs    = linSpaceV q0 q1 n
        minimizingqs = multidimensionalMinimize func (pathToList initialqs)
        func         = parametricPathAction lagrangian t0 q0 t1 q1 . listToPath

        pathToList   = concatMap toCoords
        listToPath   = map fromCoords . cut (dim q0)

    in  makePath t0 q0 t1 q1 (listToPath minimizingqs)

cut :: Int -> [a] -> [[a]]
cut n [] = []
cut n xs = take n xs : cut n (drop n xs)

-- |Compute the action of a particle along a parametric path. The user should supply the lagrangian,
--the initial and final times, and a list of sample points along the path to be integrated. A path
--will be constructed by interpolating between the sample points, and the final action computed
--by integrating over this path.
parametricPathAction :: (HasBasis v, Scalar v ~ Real) =>
                        (Local v -> Real)       -- lagrangian
                     -> Real                    -- initial time
                     -> v                       -- initial position
                     -> Real                    -- final time
                     -> v                       -- final position
                     -> [v]                     -- path
                     -> Real                    -- action
parametricPathAction l t0 q0 t1 q1 qs = lagrangianAction l path t0 t1
    where
        path = makePath t0 q0 t1 q1 qs

-- |Given an initial and final time, and a list of points sampled equally along a path, create a
--continuous path by interpolating between the sample points.
makePath :: (VectorSpace v, s ~ Scalar v, Fractional s, Ord s) =>
            s           -- initial time
         -> v           -- initial position
         -> s           -- final time
         -> v           -- final position
         -> [v]         -- intermediate path
         -> (s -> v)    -- path
makePath t0 q0 t1 q1 qs =
    let n  = length qs
        ts = linSpace t0 t1 n
     in linearInterpolation ([t0] ++ ts ++ [t1]) ([q0] ++ qs ++ [q1])

-- |Return a list of @n@ linearly spaced points between @lo@ and @hi@.
linSpace :: (Fractional s) => s -> s -> Int -> [s]
linSpace lo hi n = map (\n -> lo + fromIntegral n * dv) [1 .. n]
    where
        nr = fromIntegral (n + 1) 
        dv = (1/nr) * (hi - lo)

-- |Return a list of @n@ vectors linearly spaced between @lo@ and @hi@.
linSpaceV :: (VectorSpace v, s ~ Scalar v, Fractional s) => v -> v -> Int -> [v]
linSpaceV lo hi n = map (\n -> lo <+> fromIntegral n *> dv) [1 .. n]
    where
        nr = fromIntegral (n + 1) 
        dv = (1/nr) *> (hi <-> lo)

-- |Return a path through a vector space using linear interpolation
linearInterpolation :: (VectorSpace v, s ~ Scalar v, Fractional s, Ord s) => [s] -> [v] -> s -> v
linearInterpolation xs vs x = go (head xs) (head vs) (tail xs) (tail vs)
    where
        go x0 v0 (x1:xs) (v1:vs)
            | x < x1 || null xs = linearInterp1 x0 v0 x1 v1 x
            | otherwise         = go x1 v1 xs vs

        linearInterp1 t0 v0 t1 v1 t = c0 *> v0 <+> c1 *> v1
            where
                c0 = (t1 - t) / dt
                c1 = (t - t0) / dt
                dt = t1 - t0

-- Parametric path for the harmonic oscillator using findPath.

qHarmonicApprox :: Double -> Double
qHarmonicApprox = findPath (lHarmonic 1.0 1.0) t0 q0 t1 q1 nPoints
    where
        t0      = 0.0
        q0      = 1.0
        t1      = pi / 2
        q1      = 0.0
        nPoints = 3

qHarmonicExact :: Double -> Double
qHarmonicExact = cos

-- |Definition of delta (ex 1.8). Note that u, v can be functions!
delta :: (VectorSpace u, VectorSpace v, s ~ Scalar u, s ~ Scalar v, Fractional s) =>
         u             -- path variation
      -> (u -> t -> v) -- functional on paths
      -> u             -- true path
      -> t -> v        -- derivative of path
delta eta f q t = ( (f (q <+> eps *> eta) <-> f q ) </ eps) t
    where
        eps = 1e-6

--------------- playground

f :: InnerSpace v => v -> Scalar v
f v = dot v v

v1 :: V3 Expr
v1 = V3 x y z

v2 :: V3 Double
v2 = V3 1.0 2.0 3.0

derivAtBasis :: (HasBasis a, HasBasis b, VectorSpace c, Scalar b ~ Scalar c) =>
  ((a :> a) -> (b :> c)) -> a -> (c, [c])
derivAtBasis f x = (value f'df, map (deriv f'df . basisValue) enumerate)
    where
        f'df = f (idD x)

--local0 :: Local (V3 Expr :> V3 Expr)
--local0 = Local (constD t) (constD (V3 x y z)) (constD (V3 x' y' z'))

--local1 :: Local (V3 Expr :> V3 Expr)
--local1 = Local (constD t) (idD (V3 x y z)) (constD (V3 x' y' z'))

--local2 :: Local (V3 Expr :> V3 Expr)
--local2 = Local (constD t) (constD (V3 x y z)) (idD (V3 x' y' z'))

--local = Local

--partial :: (HasBasis a) => Int -> Local a -> Local (a :> a)
--partial 0 (Local t x v) = Local (idD t) (constD x) (constD v)
--partial 1 (Local t x v) = Local (constD t) (idD x) (constD v)
--partial 2 (Local t x v) = Local (constD t) (constD x) (idD v)

--local1 = partial 1 (Local t x x')
--local2 = partial 2 (Local t x x')

--lHarmonic2 :: (InnerSpace v, Fractional (Scalar v)) => Scalar v -> Local v -> Scalar v
--lHarmonic2 k local = (-0.5) * k * (dot q q)
--    where q = position local

local1 = Local (constD t) (idD (V3 x y z)) (constD (V3 x' y' z'))
local2 = Local (constD t) (constD (V3 x y z)) (idD (V3 x' y' z'))

lagrangian1 :: Local (V3 Expr :> V3 Expr) -> V3 Expr :> Expr
--lagrangian1 :: Local (Expr :> Expr) -> Expr :> Expr
lagrangian1 = lFreeParticle (constD m)

lagrangian2 :: Local (V3 Expr :> V3 Expr) -> V3 Expr :> Expr
--lagrangian2 :: Local (Expr :> Expr) -> Expr :> Expr
lagrangian2 = lHarmonic (constD m) (constD k)

--lagrangian3 :: Local (V3 Expr :> V3 Expr) -> V3 Expr :> Expr
--lagrangian3 :: Local (Expr :> Expr) -> Expr :> Expr
--lagrangian3 = lHarmonic2 (constD k)

test :: (Expr, V3 Expr)
--test :: (Expr, Expr)
test = let r = lagrangian2 local2
           s = lagrangian2 local1
        in (value r, fromCoords (map (deriv (r-s) . basisValue) enumerate))

main = do
  let (result, v3result) = test
  print result
  print v3result
  putStrLn "Tests complete."
