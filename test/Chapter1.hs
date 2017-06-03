
{-# LANGUAGE OverloadedStrings, TypeFamilies, TypeOperators,
             FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}
{-
SICM Chapter 1 Examples
-}

module Chapter1 where

import Prelude hiding ((*>), Real)
import Control.Applicative hiding ((*>), (<*))

import HCMUtils.Expr
import HCMUtils.AD2
import HCMUtils.Differentiation
import HCMUtils.Integration
import HCMUtils.Optimization

import HCMUtils.Basis
import HCMUtils.Differentiation
import HCMUtils.VectorSpace
import HCMUtils.InnerSpace
import HCMUtils.Vector2
import HCMUtils.Vector3


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

lFreeParticle :: (InnerSpace v, Fractional (Scalar v)) => Scalar v -> Local v -> Scalar v
lFreeParticle mass local = 0.5 * mass * (dot v v)
    where v = velocity local

-- |Coordinate function.
q :: Expr -> V3 Expr
q = up $ V3 (literalFunction x)
            (literalFunction y)
            (literalFunction z)

data Local v = Local !(Scalar v) !v !v

position :: Local v -> v
position (Local _ pos _) = pos

velocity :: Local v -> v
velocity (Local _ _ vel) = vel

-- |Lift any functor of functions to a function that acts to produce a functor.
up :: Functor f => f (a -> b) -> a -> f b
up fs t = fmap ($t) fs

-- |Gamma takes a coordinate path and returns a function of time that gives the
-- local tuple
gamma :: (HasBasis v, Differentiable (Scalar v)) => (Scalar v -> v) -> Scalar v -> Local v
gamma q t = Local t (q t) (d q t)

main = do
  print $ q t
  print $ (d q) t
  print $ (d q) t
  -- print $ (gamma q)
