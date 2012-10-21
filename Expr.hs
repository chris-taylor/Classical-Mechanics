{-# LANGUAGE OverloadedStrings #-}

module Expr2 where

import           GHC.Exts (IsString (..))
import           Prelude hiding (Real)
import qualified Data.Map  as Map
import qualified Data.List as List

type Real = Double

newtype Expr = Expr (Map.Map Prod Real) deriving (Eq,Ord)

newtype Prod = Prod (Map.Map Atom Int) deriving (Eq,Ord)

data Atom = Var String
          | App String Expr
          deriving (Eq,Ord)

constE :: Real -> Expr
constE c = Expr $ Map.singleton (Prod $ Map.empty) c

varE :: String -> Expr
varE v = Expr $ Map.singleton (Prod $ Map.singleton (Var v) 1) 1

atomE :: Atom -> Expr
atomE a = Expr $ Map.singleton (Prod $ Map.singleton a 1) 1

showAtom :: Atom -> String
showAtom (Var s)   = s
showAtom (App f s) = f ++ "(" ++ show s ++ ")"

showProd :: Prod -> String
showProd (Prod m) = concatMap shw $ Map.toList m
    where
        shw (e,n) = showAtom e ++ (case n of
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

showExpr :: Expr -> String
showExpr (Expr m) = List.intercalate " + " . map shw $ Map.toList m
    where
        shw (p,n) = pre ++ showProd p
            where
                pre = case n of
                        1    -> ""
                        (-1) -> "-"
                        n    -> show n

instance Show Expr where
    show = showExpr

instance IsString Expr where
    fromString = varE

instance Num Expr where
    Expr as + Expr bs =
        Expr $ Map.unionWith (+) as bs

    Expr as * Expr bs =
        Expr $ Map.fromListWith (+) [ mul a b | a <- Map.toList as, b <- Map.toList bs ]
        where
            mul (Prod xs, a) (Prod ys, b) = (Prod $ Map.unionWith (+) xs ys, a * b)

    negate (Expr as) = Expr $ Map.map negate as

    fromInteger = constE . fromInteger

instance Fractional Expr where
    fromRational = constE . fromRational

    recip = undefined

instance Floating Expr where
    pi   = constE pi
    exp  = atomE . App "exp"
    log  = atomE . App "log"
    sqrt = atomE . App "sqrt"
    cos  = atomE . App "cos"
    sin  = atomE . App "sin"
    tan  = atomE . App "tan"
    acos = atomE . App "acos"
    asin = atomE . App "asin"
    atan = atomE . App "atan"
    sinh = atomE . App "sinh"
    cosh = atomE . App "cosh"
    tanh = atomE . App "tanh"
    acosh = atomE . App "acosh"
    asinh = atomE . App "asinh"
    atanh = atomE . App "atanh"

------------------------------
-- Atomic variables
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
