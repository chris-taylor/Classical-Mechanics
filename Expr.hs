{-# LANGUAGE OverloadedStrings #-}

module Expr where

import GHC.Exts (IsString (..))
import Prelude hiding (Real,(^))

import qualified Data.Map as Map

------------------------------
-- Type Synonyms
------------------------------

type Var  = String
type Real = Float

------------------------------
-- Pow typeclass
------------------------------

class Num a => Pow a where
    (^) :: Integral b => a -> b -> a

instance Pow Float where
    a ^ b = a Prelude.^^ b

instance Pow Double where
    a ^ b = a Prelude.^^ b

instance Pow Expr where
    a ^ b = Pow a (fromIntegral b)

------------------------------
-- Symbolic expressions
------------------------------

data Expr = Var Var
          | Num Real
          | Neg Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Int
          | Lit Var Expr                   -- literal function application
          | App Var (Real -> Real) Expr    -- function application with lookup

instance Show Expr where
    show (Var v) = v
    show (Num n) = show n
    show (Neg a) = "-" ++ show a
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Sub a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Div a b) = "(" ++ show a ++ " / " ++ show b ++ ")"
    show (Pow a b) = show a ++ "^" ++ show b
    show (Lit f x) = f ++ " " ++ show x
    show (App f _ x) = f ++ "(" ++ show x ++ ")"

instance Eq Expr where
    Var a == Var b = a == b
    Num a == Num b = a == b
    _     == _     = False

instance Ord Expr where
    compare (Num a) (Num b) = compare a b
    compare  expr1   expr2  = compare (simplify expr1) (simplify expr2)

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

instance Floating Expr where
    pi   = Num pi
    exp  = App "exp" exp
    log  = App "log" log
    sqrt = App "sqrt" sqrt
    cos  = App "cos" cos
    sin  = App "sin" sin
    tan  = App "tan" tan
    acos = App "acos" acos
    asin = App "asin" asin
    atan = App "atan" atan
    sinh = App "sinh" sinh
    cosh = App "cosh" cosh
    tanh = App "tanh" tanh
    acosh = App "acosh" acosh
    asinh = App "asinh" asinh
    atanh = App "atanh" atanh

instance IsString Expr where
    fromString = Var

------------------------------
-- Expression Utils
------------------------------

isLiteral :: Expr -> Bool
isLiteral expr = case expr of
    Var _ -> True
    Num _ -> True
    _     -> False

------------------------------
-- Evaluate expressions
------------------------------

type Environment = Map.Map Var Expr

eval :: Environment -> Expr -> Real
eval env (Num n) = n
eval env (Var v) = eval env (env Map.! v)
eval env (Neg a) = negate (eval env a)
eval env (Add a b) = eval env a + eval env b
eval env (Sub a b) = eval env a - eval env b
eval env (Mul a b) = eval env a * eval env b
eval env (Div a b) = eval env a / eval env b
eval env (Pow a n) = eval env a ^^ n
eval env (App _ f a) = f (eval env a)

------------------------------
-- Simplification of expressions
------------------------------

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

simplify (Lit f e) = Lit f (simplify e)
simplify (App f g e) = App f g (simplify e)

------------------------------
-- Derivative of expressions
------------------------------

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

        go (App f g e) = case f of
            "exp" -> go e `Mul` App "exp" exp e
            "log" -> go e `Mul` Div 1 e
            "sin" -> go e `Mul` App "cos" cos e
            "cos" -> go e `Mul` Neg (App "sin" sin e)

------------------------------
-- Useful synonyms
------------------------------

m, t, x, y, z, x', y', z' :: Expr
m = "m"
t = "t"
x = "x"
y = "y"
z = "z"
x' = "x'"
y' = "y'"
z' = "z'"
