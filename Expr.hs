{-# LANGUAGE OverloadedStrings #-}

module Expr where

import           GHC.Exts (IsString (..))
import           Prelude hiding (Real)
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
          | Div Expr Expr
          | App String (Real -> Real) Expr


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
            shw _ (Sum  []) = "EMPTYPRODUCT"
            shw _ (Prod []) = "EMPTYSUM"

            shw n (App f _ val) = f ++ "(" ++ shw n val ++ ")"

            shw 0 (Div a b)  = case (constD a, constD b) of
                (Just x, Just y) -> show (x/y)
                (_, _)           -> shw 1 a ++ " / " ++ shw 1 b

            shw 0 (Sum vals) =
                concat $ List.intersperse " + " $ map show $ List.sortBy cmp vals
                where
                    Prod a `cmp` Prod b = length b `compare` length a
                    _      `cmp` Prod _ = GT
                    Prod _ `cmp` _      = LT
                    _      `cmp` _      = EQ

            shw 0 a = shw 1 a

            shw 1 (Prod vals) =
                pre ++ (concat $ List.intersperse "*" $
                    map (showWithPow . lengthSnd) $ collectCopies nonConsts)

                where
                    isConst (Num a) = True
                    isConst _       = False

                    consts    = map (\(Num n) -> n) $ filter isConst vals
                    nonConsts = filter (not . isConst) vals

                    pre = if null consts || product consts == 1
                            then ""
                            else case product consts of
                                1    -> ""
                                (-1) -> "-"
                                _    -> show (product consts)

                    lengthSnd (a,b) = (a,length b)

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

--The 'Eq' instance just uses symbolic equality.
instance Eq Expr where
    a == b = a === b

--The 'Ord' instance is useful for storing Exprs in Maps, Sets etc. It is not
--used for storing symbolic inequalities, which is currently not supported in
--this module.
instance Ord Expr where
    Num a     `compare` Num b     = a `compare` b
    Num _     `compare` _         = LT
    Var a     `compare` Var b     = a `compare` b
    Var _     `compare` _         = LT
    Sum a     `compare` Sum b     = a `compare` b
    Sum _     `compare` _         = LT
    Prod a    `compare` Prod b    = a `compare` b
    Prod _    `compare` _         = LT
    Div a c   `compare` Div b d   = (a,c) `compare` (b,d)
    Div _ _   `compare` _         = LT
    App f _ a `compare` App g _ b = (f,a) `compare` (g,b)

instance Symbolic Expr where
    constC         = Num
    constD (Num n) = Just n
    constD _       = Nothing

    varC           = Var
    varD (Var s)   = Just s
    varD _         = Nothing

instance SymbolicSum Expr where
    sumC         = Sum
    sumD (Sum l) = Just l
    sumD _       = Nothing

instance SymbolicProd Expr where
    prodC          = Prod
    prodD (Prod l) = Just l
    prodD _        = Nothing

instance SymbolicDiv Expr where
    divC           = Div
    divD (Div a b) = Just (a,b)
    divD _         = Nothing

instance SymbolicRealFun Expr where
    funC str f a       = App str f a
    funD (App str f a) = Just (str,f,a)
    funD _             = Nothing

instance Num Expr where
    fromInteger = Num . fromInteger
    a + b       = sumC'  [a,b]
    a * b       = prodC' [a,b]
    negate      = ((Num (-1))*)

    abs    _ = undefined
    signum _ = undefined

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

--------------------------------
---- Evaluate expressions
--------------------------------

type Environment = Map.Map Var Expr

eval :: Environment -> Expr -> Real
eval env (Num n) = n
eval env (Var v) = eval env (env Map.! v)
eval env (Sum  as) = sum $ map (eval env) as
eval env (Prod as) = product $ map (eval env) as
eval env (Div a b) = eval env a / eval env b
eval env (App _ f a) = f (eval env a)

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
