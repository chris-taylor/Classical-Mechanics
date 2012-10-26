module Enumerable where

import Trio

class Enumerable a where
    enumerate :: [a]

-- Instances

instance Enumerable () where
    enumerate = [()]

instance Enumerable Bool where
    enumerate = [False, True]

instance (Enumerable a, Enumerable b) => Enumerable (a,b) where
    enumerate = [ (a,b) | a <- enumerate, b <- enumerate ]

instance (Enumerable a, Enumerable b, Enumerable c) => Enumerable (a,b,c) where
    enumerate = [ (a,b,c) | a <- enumerate, b <- enumerate, c <- enumerate ]

instance (Enumerable a, Enumerable b) => Enumerable (Either a b) where
    enumerate = map Left enumerate ++ map Right enumerate

instance (Enumerable a, Enumerable b, Enumerable c) => Enumerable (Trio a b c) where
    enumerate = map First enumerate ++ map Second enumerate ++ map Third enumerate