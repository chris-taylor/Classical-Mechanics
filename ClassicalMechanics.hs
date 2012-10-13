{-# LANGUAGE OverloadedStrings #-}

module ClassicalMechanics where

import Prelude hiding (Real,(^))
import GHC.Exts (IsString (..))

-- Pow type

class Num a => Pow a where
    (^) :: Integral b => a -> b -> a

instance Pow Float where
    a ^ b = a Prelude.^^ b

instance Pow Double where
    a ^ b = a Prelude.^^ b

instance Pow Expr where
    a ^ b = Pow a (fromIntegral b)

-- Symbol type

data Expr = Var Var
          | Num Real
          | Neg Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Int
          deriving (Eq)

instance Show Expr where
    show (Var v) = v
    show (Num n) = show n
    show (Neg a) = "-" ++ show a
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Sub a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Div a b) = "(" ++ show a ++ " / " ++ show b ++ ")"
    show (Pow a b) = show a ++ "^" ++ show b

instance Num Expr where
    (+) = Add
    (-) = Sub
    (*) = Mul
    negate   = Neg
    signum _ = undefined
    abs    _ = undefined
    fromInteger = Num . fromInteger

instance Fractional Expr where
    (/) = Div
    fromRational = Num . fromRational

instance IsString Expr where
    fromString = Var

-- Simplification of algebraic expressions

simplify :: Expr -> Expr
simplify (Var v) = Var v
simplify (Num n) = Num n

simplify (Neg e) = case simplify e of
    Neg a -> simplify a
    other -> Neg other

simplify (Add e1 e2) = case (simplify e1, simplify e2) of
    (Num 0, a) -> a
    (a, Num 0) -> a
    (Num a, Num b) -> Num (a + b)
    (a,b) -> Add a b

simplify (Sub e1 e2) = case (simplify e1, simplify e2) of
    (Num 0, a) -> Neg a
    (a, Num 0) -> a
    (Num a, Num b) -> Num (a - b)
    (a,b) -> Sub a b

simplify (Mul e1 e2) = case (simplify e1, simplify e2) of
    (Num 0, _) -> Num 0
    (_, Num 0) -> Num 0
    (Num 1, a) -> a
    (a, Num 1) -> a
    (Num a, Num b) -> Num (a * b)
    (a,b) -> Mul a b

simplify (Div e1 e2) = case (simplify e1, simplify e2) of
    (Num 0, _) -> Num 0
    (_, Num 0) -> error "Division by zero!"
    (a, Num 1) -> a
    (Num a, Num b) -> Num (a / b)
    (a,b) -> Div a b

simplify (Pow e1 n) = case (simplify e1, n) of
    (_, 0) -> Num 1
    (a, 1) -> a
    (Num 0, _) -> Num 0
    (Num 1, _) -> Num 1
    (Num a, n) -> Num (a ^ n)
    (a,n) -> Pow a n

-- Derivatives

deriv :: Expr -> Expr -> Expr
deriv (Var x) expr = simplify (go expr)
    where
        go (Num _) = Num 0
        go (Var y) = if x == y then Num 1 else Num 0
        go (Neg a) = Neg (go a)
        go (Add a b) = go a `Add` go b
        go (Sub a b) = go a `Sub` go b
        go (Mul a b) = (a `Mul` go b) `Add` (go a `Mul` b)
        go (Div a b) = ((b `Mul` go a) `Sub` (a `Mul` go b)) `Div` Pow b 2
        go (Pow a n) = Num (fromIntegral n) `Mul` Pow a (n-1)

-- Vector type

type Var  = String
type Real = Float

data Vector a = V !a !a !a deriving (Eq,Show)

dot :: Num a => Vector a -> Vector a -> a
dot (V x y z) (V x' y' z') = x * x' + y * y' + z * z'

-- Useful synonyms

x, y, z, x', y', z' :: Expr
x = "x"
y = "y"
z = "z"
x' = "x'"
y' = "y'"
z' = "z'"

coord = Local (V x y z) (V x' y' z')

-- Local coordinates

data Local a = Local !(Vector a) !(Vector a) deriving (Eq,Show)

position :: Local a -> Vector a
position (Local pos _) = pos

velocity :: Local a -> Vector a
velocity (Local _ vel) = vel

-- Free particle

type Mass = Real

lagrangianFreeParticle :: Fractional a => a -> Local a -> a
lagrangianFreeParticle mass local = 0.5 * mass * (v `dot` v)
    where v = velocity local

