{-# LANGUAGE OverloadedStrings #-}

module Expr where

import           GHC.Exts (IsString (..))
import           Prelude hiding (Real,(^))
import qualified Data.Map  as Map
import qualified Data.List as List

import Symbolic

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
          | Sum  [Expr]
          | Prod [Expr]
          | Sin Expr
          | Cos Expr

-- |Utility function to collect all identical elements, and return
--them together with a list giving their positions in the original list.
--For example,
--
--  >>> collectCopies "ababa"
--  [('a',[0,2,4]),('b',[1,3])]
--
--This is particularly useful for grouping algebraic expresions to be printed.
collectCopies :: (Eq a) => [a] -> [(a, [Int])]
collectCopies vars = map (\v -> (v, positions 0 v vars)) $ List.nub vars
    where
        positions :: Eq a => Int -> a -> [a] -> [Int]
        positions _     _     []     = []
        positions shift match (x:xs) =
            if x == match
                then shift : positions (shift + 1) match xs
                else positions (shift + 1) match xs

-- |Show instance for expressions. This is lifted from Algebra.HaskSymb.
instance Show Expr where
    show = shw 0
        where
            shw :: Int -> Expr -> String
            shw _ (Sum  []) = "0"
            shw _ (Prod []) = "1"

            shw n (Sin val) = "sin(" ++ shw n val ++ ")"
            shw n (Cos val) = "cos(" ++ shw n val ++ ")"

            shw 0 (Sum vals) =
                concat $ List.intersperse " + " $ map (shw 0) $
                                                reverse $ List.sortBy cmp vals
                where
                    Prod a `cmp` Prod b = length a `compare` length b
                    _      `cmp` Prod _ = LT
                    Prod _ `cmp` _      = GT
                    _      `cmp` _      = EQ

            shw 0 a = shw 1 a

            shw 1 (Prod vals) =
                pre ++ (concat $ List.intersperse "*" $
                    map (showWithPow . lengthifySecond) $ collectCopies nonConsts)

                where
                    isConst (Num a) = True
                    isConst _       = False

                    consts    = map (\(Num n) -> n) $ filter isConst vals
                    nonConsts = filter (not . isConst) vals

                    pre = if null consts || product consts == 1
                            then ""
                            else show (product consts)

                    lengthifySecond (a,b) = (a,length b)

                    showWithPow (a,n) = shw 1 a ++ (case n of
                                                        1 -> ""
                                                        2 -> "²"
                                                        3 -> "³"
                                                        4 -> "⁴"
                                                        5 -> "⁵"
                                                        6 -> "⁶"
                                                        7 -> "⁷"
                                                        8 -> "⁸"
                                                        9 -> "⁹"
                                                        n -> "^" ++ show n)

            shw 1 (Num a) = show a
            shw 1 (Var s) = s
            shw 1 a       = "(" ++ shw 0 a ++ ")"

instance Eq Expr where
    a == b = a === b

instance Symbolic Expr where
    constC         = Num
    constD (Num n) = Just n
    constD _       = Nothing

    varC         = Var
    varD (Var s) = Just s
    varD _       = Nothing

instance SymbolicSum Expr where
    sumC         = Sum
    sumD (Sum l) = Just l
    sumD _       = Nothing

instance SymbolicProd Expr where
    prodC          = Prod
    prodD (Prod l) = Just l
    prodD _        = Nothing

instance Num Expr where
    fromInteger = Num . fromInteger
    a + b = sumC'  [a,b]
    a * b = prodC' [a,b]
    negate = ((-1)*)

    abs    _ = undefined
    signum _ = undefined


----Minimal 'Eq' instance (we can't derive it since there is no derivable 'Eq'
----instance for @Real -> Real@).
--instance Eq Expr where
--    Var a == Var b = a == b
--    Num a == Num b = a == b
--    _     == _     = False

----The 'Ord' instance is necessary to do definite integrals, where the limits
----can be supplied as numbers or as expressions representing numbers.
--instance Ord Expr where
--    compare (Num a) (Num b) = compare a b
--    compare  expr1   expr2  = compare (simplify expr1) (simplify expr2)

----Since we are only concerned with smooth functions, I don't bother to give a
----representation for the 'abs' and 'signum' functions. Using them will cause
----the program to crash. I should probably hide them from user-facing code.
--instance Num Expr where
--    (+) = Add
--    (-) = Sub
--    (*) = Mul
--    signum _ = undefined
--    abs    _ = undefined
--    fromInteger = Num . fromInteger

--instance Fractional Expr where
--    (/) = Div
--    fromRational = Num . fromRational

--instance Floating Expr where
--    pi   = Num pi
--    exp  = App "exp" exp
--    log  = App "log" log
--    sqrt = App "sqrt" sqrt
--    cos  = App "cos" cos
--    sin  = App "sin" sin
--    tan  = App "tan" tan
--    acos = App "acos" acos
--    asin = App "asin" asin
--    atan = App "atan" atan
--    sinh = App "sinh" sinh
--    cosh = App "cosh" cosh
--    tanh = App "tanh" tanh
--    acosh = App "acosh" acosh
--    asinh = App "asinh" asinh
--    atanh = App "atanh" atanh

instance IsString Expr where
    fromString = Var

---- |Literal expressions (numbers and variables) don't need to be surrounded by
----parentheses when we print them, so we provide a convenience function that
----identifies those special cases.
--isLiteral :: Expr -> Bool
--isLiteral expr = case expr of
--    Var _ -> True
--    Num _ -> True
--    _     -> False

--------------------------------
---- Evaluate expressions
--------------------------------

--type Environment = Map.Map Var Expr

--empty :: Environment
--empty = Map.empty

--eval :: Environment -> Expr -> Real
--eval env (Num n) = n
--eval env (Var v) = eval env (env Map.! v)
--eval env (Add a b) = eval env a + eval env b
--eval env (Sub a b) = eval env a - eval env b
--eval env (Mul a b) = eval env a * eval env b
--eval env (Div a b) = eval env a / eval env b
--eval env (Pow a n) = eval env a ^^ n
--eval env (App _ f a) = f (eval env a)

--------------------------------
---- Simplification of expressions
--------------------------------

--simplify :: Expr -> Expr
--simplify (Var v) = Var v
--simplify (Num n) = Num n

--simplify (Add e1 e2) = case (simplify e1, simplify e2) of
--    (Num 0, a) -> a
--    (a, Num 0) -> a
--    (Num a, Num b) -> Num (a + b)
--    (a,b) -> Add a b

--simplify (Sub e1 e2) = case (simplify e1, simplify e2) of
--    (Num 0, a) -> Mul (Num (-1)) a
--    (a, Num 0) -> a
--    (Num a, Num b) -> Num (a - b)
--    (a,b) -> Sub a b

--simplify (Mul e1 e2) = case (simplify e1, simplify e2) of
--    (Num 0, _) -> Num 0
--    (_, Num 0) -> Num 0
--    (Num 1, a) -> a
--    (a, Num 1) -> a
--    --(Num (-1), a) -> case a of
--    --    Num n -> Num (-n)
--    --    a     -> negate a
--    --(a, Num (-1)) -> case a of
--    --    Num n -> Num (-n)
--    --    a     -> negate a
--    (Num a, Num b) -> Num (a * b)
--    (a,b) -> Mul a b

--simplify (Div e1 e2) = case (simplify e1, simplify e2) of
--    (Num 0, _) -> Num 0
--    (_, Num 0) -> error "Division by zero!"
--    (a, Num 1) -> a
--    (Num a, Num b) -> Num (a / b)
--    (a,b) -> Div a b

--simplify (Pow e1 n) = case (simplify e1, n) of
--    (_, 0) -> Num 1
--    (a, 1) -> a
--    (Num 0, _) -> Num 0
--    (Num 1, _) -> Num 1
--    (Num a, n) -> Num (a ^ n)
--    (a,n) -> Pow a n

--simplify (Lit f e)   = Lit f (simplify e)
--simplify (App f g e) = App f g (simplify e)

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
