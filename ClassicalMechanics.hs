module ClassicalMechanics where

import Expr
import Prelude hiding (Real,(^))

------------------------------
-- Vector type
------------------------------

data Vector a = V !a !a !a deriving (Eq)

instance Show a => Show (Vector a) where
    show (V x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

instance Functor Vector where
    fmap f (V x y z) = V (f x) (f y) (f z)

--instance Applicative Vector where
--    pure a = V a a a
--    V f g h <*> V x y z = V (f x) (g y) (h z)

dot :: Num a => Vector a -> Vector a -> a
dot (V x y z) (V x' y' z') = x * x' + y * y' + z * z'

------------------------------
-- Local coordinates
------------------------------

data Local f a = Local !a !(f a) !(f a) deriving (Eq,Show)

time :: Local f a -> a
time (Local t _ _) = t

position :: Local f a -> f a
position (Local _ pos _) = pos

velocity :: Local f a -> f a
velocity (Local _ _ vel) = vel

coord = Local t (V x y z) (V x' y' z')

-- |Lift any functor of functions to a function that acts to produce a functor.
up :: Functor f => f (a -> b) -> a -> f b
up fs t = fmap ($t) fs

-- |Coordinate function.
q :: Expr -> Vector Expr
q = up $ V (literalFunction x)
           (literalFunction y)
           (literalFunction z)

-- |Create a literal symbolic function.
literalFunction :: Expr -> Expr -> Expr
literalFunction (Var f) x = Lit f x

------------------------------
-- Symbolic derivatives
------------------------------

d :: Functor f => (Expr -> f Expr) -> Expr -> f Expr
d q = fmap (Lit "D") . q

------------------------------
-- Local coordinate function
------------------------------

gamma :: Functor f => (Expr -> f Expr) -> Expr -> Local f Expr
gamma q t = Local t (q t) (d q t)

------------------------------
-- Free particle
------------------------------

type Mass = Real

lagrangianFreeParticle :: Fractional a => a -> Local Vector a -> a
lagrangianFreeParticle mass local = 0.5 * mass * (dot v v)
    where v = velocity local

------------------------------
-- Lagrangian action function
------------------------------

simpleIntegral :: (Fractional a, Ord a) => a -> (a -> a) -> a -> a -> a
simpleIntegral dx f a b = go 0 (a+dx/2)
    where
        go result x = if x > b
            then dx * result
            else go (result + f x) (x + dx)

definiteIntegral :: Num a => (a -> a) -> a -> a -> a
definiteIntegral = undefined

lagrangianAction :: Functor f => (Local f Expr -> Expr) -> (Expr -> f Expr) -> Expr -> Expr -> Expr
lagrangianAction l q t1 t2 = definiteIntegral (l . gamma q) t1 t2

-- Test path

testPath t = V (4 * t + 7) (3 * t + 5) (2 * t + 1)
