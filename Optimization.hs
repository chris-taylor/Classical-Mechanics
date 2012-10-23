module Optimization ( minimize ) where

import           Numeric.LinearAlgebra
import qualified Numeric.GSL.Minimization as GSL

minimize :: (Double -> Double) -> Double -> Double -> (Double, Double)
minimize f a b = (head xopt, f (head xopt))
    where
        (xopt,path) = GSL.minimize GSL.NMSimplex 1e-10 1000 sizes (f . head) xi
        xi          = [0.5 * (a + b)]
        sizes       = [1]

