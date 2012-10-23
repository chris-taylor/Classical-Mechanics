module Optimization ( minimize, multidimensionalMinimize ) where

import           Numeric.LinearAlgebra
import qualified Numeric.GSL.Minimization as GSL

minimize :: (Double -> Double) -> Double -> Double -> (Double, Double)
minimize f a b = (head xopt, f (head xopt))
    where
        (xopt,path) = GSL.minimize GSL.NMSimplex 1e-10 1000 sizes (f . head) xi
        xi          = [0.5 * (a + b)]
        sizes       = [b - a]

multidimensionalMinimize :: ([Double] -> Double) -> [Double] -> [Double]
multidimensionalMinimize f xi = xopt
    where
        (xopt,path) = GSL.minimize GSL.NMSimplex 1e-6 1000 sizes f xi
        sizes       = replicate (length xi) 1
