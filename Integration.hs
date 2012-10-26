module Integration ( definiteIntegral ) where

import qualified Data.List as L
import qualified Numeric.GSL.Integration as GSL

-- |This function applies the Gauss-Kronrod 10-point, 21-point, 43-point and
--87-point integration rules in succession until an estimate of the integral
--of @f@ over @(a,b)@ is achieved within the desired error limit.
intQNG :: (Double -> Double) -> Double -> Double -> Double
intQNG f a b = fst $ GSL.integrateQNG 1e-6 f a b

-- |This function applies the Gauss-Kronrod 21-point integration rule
--adaptively until an estimate of the integral of @f@ over @(a,b)@ is achieved
--within the desired error limits. The results are extrapolated using the
--epsilon-algorithm, which accelerates the convergence of the integral in the
--presence of discontinuities and integrable singularities.
intQAGS :: (Double -> Double) -> Double -> Double -> Double
intQAGS f a b = fst $ GSL.integrateQAGS 1e-6 1000 f a b

-- |This functions returns a simple pointwise integration of @f@ over @(a,b)@
--using 100 intermediate points.
simpleIntegral :: (Double -> Double) -> Double -> Double -> Double
simpleIntegral f a b = dx * go xs
    where
        n  = 100
        xs = map (\i -> a + (i + 0.5) * dx ) [ 0 .. n-1 ]
        dx = (b - a) / n
        go = L.foldl' (\accum x -> accum + f x) 0

definiteIntegral = intQAGS
