{-# LANGUAGE OverloadedStrings #-}

module Expr where

import           GHC.Exts (IsString (..))
import           Prelude hiding (Real)
import qualified Data.Map  as Map
import qualified Data.List as List

type Real = Double
type Var  = String

newtype Expr = Expr (Map.Map Prod Real) deriving (Eq,Ord)

newtype Prod = Prod (Map.Map Atom Int) deriving (Eq,Ord)

data Atom = Var Var
          | App String Expr
          deriving (Eq,Ord,Show)

constE :: Real -> Expr
constE c = Expr $ Map.singleton (Prod $ Map.empty) c

atomE :: Atom -> Expr
atomE a = Expr $ Map.singleton (Prod $ Map.singleton a 1) 1

varE :: String -> Expr
varE = atomE . Var

isVarA :: Atom -> Bool
isVarA (Var _) = True
isVarA _       = False

getVarA :: Atom -> Var
getVarA (Var v) = v
getVarA _       = error "Not a variable!"

isConstP :: Prod -> Bool
isConstP (Prod m) = Map.null m

isConst :: Expr -> Bool
isConst (Expr m) = Map.size m == 1 && Map.size p == 0
    where
        (Prod p, _) = Map.findMin m

getConst :: Expr -> Real
getConst e@(Expr m) = if isConst e
    then c
    else error "Not a constant!"
    where
        (Prod p, c) = Map.findMin m

isAtom :: Expr -> Bool
isAtom (Expr m) = Map.size m == 1 && c == 1 && Map.size p == 1 && n == 1
    where
        (Prod p, c) = Map.findMin m
        (atom, n)   = Map.findMin p

getAtom :: Expr -> Atom
getAtom e@(Expr m) = if not (isAtom e)
    then error "Not an atom!"
    else atom
        where
            (Prod p, c) = Map.findMin m
            (atom, _)   = Map.findMin p

isVar :: Expr -> Bool
isVar e = isAtom e && isVarA (getAtom e)

getVar :: Expr -> Var
getVar e = if isVar e
    then getVarA (getAtom e)
    else error "Not a variable!"

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
showExpr (Expr m) =
    if Map.null m
        then "0"
        else List.intercalate " + " . map shw $ Map.toList m
    where
        shw (p,n) = pre ++ showProd p
            where
                pre = if isConstP p
                        then show n
                        else case n of
                                1    -> ""
                                (-1) -> "-"
                                _    -> show n

instance Show Expr where
    show = showExpr

instance IsString Expr where
    fromString = varE

instance Num Expr where
    Expr as + Expr bs =
        Expr $ Map.filter (/=0) $ Map.unionWith (+) as bs

    Expr as * Expr bs =
        Expr $ Map.fromListWith (+) [ mul a b | a <- Map.toList as, b <- Map.toList bs ]
        where
            mul (Prod xs, a) (Prod ys, b) = (Prod $ Map.unionWith (+) xs ys, a * b)

    fromInteger = constE . fromInteger

    negate (Expr as) = Expr $ Map.map negate as

    abs    _ = undefined
    signum _ = undefined

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
