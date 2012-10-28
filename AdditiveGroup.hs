module AdditiveGroup ( AdditiveGroup(..), Sum(..), sumV ) where

import Control.Applicative
import Data.Monoid ( Monoid(..) )
import Iso

infixl 6 <+>
infixl 6 <->

-- |Additive group @v@.
class AdditiveGroup v where

    -- | The zero element: identity for '<+>'
    zeroV :: v

    -- | Add vectors
    (<+>) :: v -> v -> v

    -- | Subtract vectors
    (<->) :: v -> v -> v
    u <-> v = u <+> negateV v

    -- | Additive inverse
    negateV :: v -> v
    negateV v = zeroV <-> v

-- |Sum of vectors.
sumV :: AdditiveGroup a => [a] -> a
sumV = go zeroV
    where
        go accum []     = accum
        go accum (v:vs) = go (accum <+> v) vs 

-- Numeric instances

instance AdditiveGroup Int where
    zeroV = 0
    (<+>) = (+)
    (<->) = (-)

instance AdditiveGroup Integer where
    zeroV = 0
    (<+>) = (+)
    (<->) = (-)

instance AdditiveGroup Float where
    zeroV = 0
    (<+>) = (+)
    (<->) = (-)

instance AdditiveGroup Double where
    zeroV = 0
    (<+>) = (+)
    (<->) = (-)

-- Function instance

instance AdditiveGroup v => AdditiveGroup (a -> v) where
    zeroV   = const zeroV
    f <+> g = \a -> f a <+> g a
    f <-> g = \a -> f a <-> g a

-- Tuple instances

instance AdditiveGroup () where
    zeroV     = ()
    () <+> () = ()
    () <-> () = ()

instance (AdditiveGroup g, AdditiveGroup h) => AdditiveGroup (g,h) where
    zeroV           = (zeroV, zeroV)
    (a,b) <+> (c,d) = (a <+> c, b <+> d)
    (a,b) <-> (c,d) = (a <-> c, b <-> d)

instance (AdditiveGroup g, AdditiveGroup h, AdditiveGroup i) => AdditiveGroup (g,h,i) where
    zeroV               = (zeroV, zeroV, zeroV)
    (a,b,c) <+> (d,e,f) = (a <+> d, b <+> e, c <+> f)
    (a,b,c) <-> (d,e,f) = (a <-> d, b <-> e, c <-> f)

-- Sum type 

-- |Sum data type. An alternative to the one in Data.Monoid that uses a @Num@
--instance instead of an AdditiveGroup instance.
data Sum a = Sum { getSum :: a } deriving (Eq,Ord,Read,Show,Bounded)

instance Functor Sum where
    fmap = inSum

instance Applicative Sum where
    pure  = Sum
    (<*>) = inSum2 ($)

instance AdditiveGroup a => Monoid (Sum a) where
    mempty                = Sum zeroV
    Sum a `mappend` Sum b = Sum (a <+> b)

inSum :: (a -> b) -> (Sum a -> Sum b)
inSum = getSum ~> Sum

inSum2 :: (a -> b -> c) -> (Sum a -> Sum b -> Sum c)
inSum2 = getSum ~> inSum