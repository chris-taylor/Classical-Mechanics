module Integration where

import Numeric.GSL.Integration

definiteIntegral :: (Double -> Double) -> Double -> Double -> Double
definiteIntegral f a b = fst $ integrateQNG 1e-6 f a b