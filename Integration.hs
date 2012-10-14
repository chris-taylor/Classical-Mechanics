module Integration where

import Data.List (foldl')

midPointIntegral :: Fractional a => Int -> (a -> a) -> a -> a -> a
midPointIntegral n f a b = dx * foldl' go 0 points
    where
        dx = (b - a) / fromIntegral n
        points = take n $ iterate (+dx) (a+dx/2)
        go result x = result + f x

definiteIntegral :: Fractional a => (a -> a) -> a -> a -> a
definiteIntegral = midPointIntegral 1000

-- Utils

