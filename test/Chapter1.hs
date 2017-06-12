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

-- |Lift any functor of functions to a function that acts to produce a functor.
up :: (Functor f) => f (a -> b) -> a -> f b
up fs t = fmap ($t) fs

-- up is used differently than in the book, it is only needed for deferred
-- evaluation rather than as a generic tuple constructor
upVal = up $ V2 (\x -> 2 * x) (\y-> 3 * y)
upApply1 = upVal 5
-- above is equivalent to
upApply2 val = fmap (\x -> x $ val) (V2 (\x -> 2 * x) (\y-> 3 * y))

lFreeParticle :: (InnerSpace v, Fractional (Scalar v))
  => Scalar v -> Local v -> Scalar v
lFreeParticle mass local = 0.5 * mass * (dot v v)
    where v = velocity local

-- | A generalized coordinates is a tuple of functions
-- | that can be applied to a value. As noted above, "up"
-- | reads as "wait to be applied to a value"
q :: Expr -> V3 Expr
q = up $ V3 (literalFunction "x")
            (literalFunction "y")
            (literalFunction "z")

testQ = print (q 4)

-- Takes function of time as input, returns local tuple
gamma :: (HasBasis v, Differentiable (Scalar v))
  => (Scalar v -> v) -> Scalar v -> Local v
gamma q t = Local t (q t) (d q t)

data Local v = Local !(Scalar v) !v !v

position :: Local v -> v
position (Local _ pos _) = pos

velocity :: Local v -> v
velocity (Local _ _ vel) = vel

-- main = do
--   print $ q t
--   print $ (d q) t
--   print $ (d q) t
--   -- print $ (gamma q)


-- definiteIntegral
--   :: (Double -> Double) -> Double -> Double -> Double

lagrangianAction :: (HasBasis v, Scalar v ~ Real) =>
                    (Local v -> Real)       -- lagrangian
                 -> (Real -> v)             -- particle path
                 -> Real                    -- initial time
                 -> Real                    -- final time
                 -> Real                    -- action
lagrangianAction l q t1 t2 = definiteIntegral (l . gamma q) t1 t2

testPath :: Real -> V3 Real
testPath t = V3 (4 * t + 7) (3 * t + 5) (2 * t + 1)

straightPath :: Real -> Real -> V2 Real
straightPath x t = V2 (x * t) (x * t)

simplePath = straightPath 1.0

upPath = up [testPath] 20

computeAction = lagrangianAction (lFreeParticle 1.5) testPath 1.0 10.0

-- computeAction2 = lagrangianAction (lFreeParticle 1.5) simplePath 1.0 10.0
