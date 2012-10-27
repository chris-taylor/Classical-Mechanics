module Iso where

-- |Given isomorphisms @i@ and @o@, lift the function @f@ to work with the
--isomorphism.
(~>) :: (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
(i ~> o) f = o . f . i