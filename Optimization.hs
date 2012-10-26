module Optimization ( minimize, multidimensionalMinimize ) where

import           Numeric.LinearAlgebra
import qualified Numeric.GSL.Minimization as GSL

-- |Minimize a function of a single variable. The first argument is the function
--to be minimized ; the second is the initial point.
minimize :: (Double -> Double) -> Double -> Double
minimize f xi = xopt where (xopt,_,_) = minimize' f xi

-- |Minimize a function of a single variable. The first argument is the function
--to be minimized; the second is the initial point. The return value is a tuple
--of (optimum, value of function at optimum, number of iterations).
minimize' :: (Double -> Double) -> Double -> (Double, Double, Int)
minimize' f xi = (head xopt, f (head xopt), rows path)
    where
        (xopt,path) = GSL.minimize GSL.NMSimplex 1e-4 1000 sizes (f . head) [xi]
        sizes       = [1]

-- |Minimize a function of multiple variables. The first argument is the
--function to be minimized; the second is the initial point.
multidimensionalMinimize :: ([Double] -> Double) -> [Double] -> [Double]
multidimensionalMinimize f xi = xopt where (xopt,_,_) = multidimensionalMinimize' f xi

-- |Minimize a function of multiple variables. The first argument is the
--function to be minimized; the second is the initial point. The return value
--is a tuple of (optimum, value of function at optimum, number of iterations).
multidimensionalMinimize' :: ([Double] -> Double) -> [Double] -> ([Double], Double, Int)
multidimensionalMinimize' f xi = (xopt, f xopt, rows path)
    where
        (xopt,path) = GSL.minimize GSL.NMSimplex 1e-4 1000 sizes f xi
        sizes       = replicate (length xi) 1
