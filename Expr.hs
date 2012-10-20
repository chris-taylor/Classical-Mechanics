{-# LANGUAGE OverloadedStrings #-}

module Expr where

import GHC.Exts (IsString (..))
import Prelude hiding (Real,(^))

import qualified Data.Map as Map

type Var  = String
type Real = Double

-- |Class for types which can be raised to integer powers. The reason for
--introducing this class is that the (^) function in the Prelude is implemented
--as iterated multiplication, meaning that an 'Expr' raised to a power would
--give the following result
--
--    ghci> x ^ 8
--    (((x * x) * (x * x)) * ((x * x) * (x * x)))
--
--rather than the more appealing and readable
--
--    ghci> x ^ 8
--    x^8
class Num a => Pow a where
    (^) :: Integral b => a -> b -> a

instance Pow Float where
    a ^ b = a Prelude.^^ b

instance Pow Double where
    a ^ b = a Prelude.^^ b

instance Pow Expr where
    a ^ b = Pow a (fromIntegral b)

-- |Symbolic expression data type. This type is the foundation for all symbolic
--manipulation in the package. Note that there are two versions of function
--application. The constructor 'Lit' is for functions that carry no information
--other than their name. The constructor 'App' is for Haskell functions of type
-- @Floating a => a -> a@, i.e. functions of real numbers. As well as the name
--of the function we also carry around its Haskell representation, so that the
--expression can be evaluated later.
--
--The alternative would be to just carry around the name of the function, and
--when we evaluate an expression also pass in a function environment, which
--provides a means to look up which function should be applied. I may modify
--the representation to do that at a later date if it seems sensible.
data Expr = Var Var
          | Num Real
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
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Sub a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Div a b) = "(" ++ show a ++ " / " ++ show b ++ ")"
    show (Pow a b) = show a ++ "^" ++ show b
    show (Lit f x) = f ++ " " ++ show x
    show (App f _ x) = f ++ "(" ++ show x ++ ")"

--Minimal 'Eq' instance (we can't derive it since there is no derivable 'Eq'
--instance for @Real -> Real@).
instance Eq Expr where
    Var a == Var b = a == b
    Num a == Num b = a == b
    _     == _     = False

--The 'Ord' instance is necessary to do definite integrals, where the limits
--can be supplied as numbers or as expressions representing numbers.
instance Ord Expr where
    compare (Num a) (Num b) = compare a b
    compare  expr1   expr2  = compare (simplify expr1) (simplify expr2)

--Since we are only concerned with smooth functions, I don't bother to give a
--representation for the 'abs' and 'signum' functions. Using them will cause
--the program to crash. I should probably hide them from user-facing code.
instance Num Expr where
    (+) = Add
    (-) = Sub
    (*) = Mul
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

-- |Literal expressions (numbers and variables) don't need to be surrounded by
--parentheses when we print them, so we provide a convenience function that
--identifies those special cases.
isLiteral :: Expr -> Bool
isLiteral expr = case expr of
    Var _ -> True
    Num _ -> True
    _     -> False

------------------------------
-- Evaluate expressions
------------------------------

type Environment = Map.Map Var Expr

empty :: Environment
empty = Map.empty

eval :: Environment -> Expr -> Real
eval env (Num n) = n
eval env (Var v) = eval env (env Map.! v)
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

simplify (Add e1 e2) = case (simplify e1, simplify e2) of
    (Num 0, a) -> a
    (a, Num 0) -> a
    (Num a, Num b) -> Num (a + b)
    (a,b) -> Add a b

simplify (Sub e1 e2) = case (simplify e1, simplify e2) of
    (Num 0, a) -> Mul (Num (-1)) a
    (a, Num 0) -> a
    (Num a, Num b) -> Num (a - b)
    (a,b) -> Sub a b

simplify (Mul e1 e2) = case (simplify e1, simplify e2) of
    (Num 0, _) -> Num 0
    (_, Num 0) -> Num 0
    (Num 1, a) -> a
    (a, Num 1) -> a
    --(Num (-1), a) -> case a of
    --    Num n -> Num (-n)
    --    a     -> negate a
    --(a, Num (-1)) -> case a of
    --    Num n -> Num (-n)
    --    a     -> negate a
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

simplify (Lit f e)   = Lit f (simplify e)
simplify (App f g e) = App f g (simplify e)

------------------------------
-- Derivative of expressions
------------------------------

--deriv :: Expr -> Expr -> Expr
--deriv (Var x) expr = simplify (go expr)
--    where
--        go (Num _) = Num 0
--        go (Var y) = if x == y then Num 1 else Num 0
--        go (Add a b) = go a `Add` go b
--        go (Sub a b) = go a `Sub` go b
--        go (Mul a b) = (a `Mul` go b) `Add` (go a `Mul` b)
--        go (Div a b) = ((b `Mul` go a) `Sub` (a `Mul` go b)) `Div` Pow b 2
--        go (Pow a n) = Num (fromIntegral n) `Mul` Pow a (n-1)

--        go (App f g e) = case f of
--            "exp" -> go e `Mul` App "exp" exp e
--            "log" -> go e `Mul` Div 1 e
--            "sin" -> go e `Mul` App "cos" cos e
--            "cos" -> go e `Mul` negate (App "sin" sin e)

------------------------------
-- Useful named variables
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
