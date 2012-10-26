module AdditiveGroup ( AdditiveGroup(..) ) where

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

instance (AdditiveGroup g, AdditiveGroup h) => AdditiveGroup (g,h) where
    zeroV           = (zeroV, zeroV)
    (a,b) <+> (c,d) = (a <+> c, b <+> d)
    (a,b) <-> (c,d) = (a <-> c, b <-> d)

instance (AdditiveGroup g, AdditiveGroup h, AdditiveGroup i) => AdditiveGroup (g,h,i) where
    zeroV               = (zeroV, zeroV, zeroV)
    (a,b,c) <+> (d,e,f) = (a <+> d, b <+> e, c <+> f)
    (a,b,c) <-> (d,e,f) = (a <-> d, b <-> e, c <-> f)
