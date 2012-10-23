module Latex where

import Expr
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

type Latex = String

latexAtom :: Atom -> Latex
latexAtom (Var s)   = s
latexAtom (App f s) = escape f ++ "(" ++ show s ++ ")"

latexProd :: Prod -> String
latexProd (Prod m) = List.intercalate " " $ map shw $ Map.toList m
    where
        shw (e,n) = latexAtom e ++ (case n of
                                    1 -> ""
                                    2 -> "^2"
                                    3 -> "^3"
                                    4 -> "^4"
                                    5 -> "^5"
                                    6 -> "^6"
                                    7 -> "^7"
                                    8 -> "^8"
                                    9 -> "^9"
                                    n -> "^{" ++ show n ++ "}")

latexExpr :: Expr -> String
latexExpr (Expr m) =
    if Map.null m
        then "0"
        else List.intercalate " + " . map shw $ Map.toList m
    where
        shw (p,n) = pre ++ latexProd p
            where
                pre = if isConstP p
                        then show n
                        else case n of
                                1    -> ""
                                (-1) -> "-"
                                _    -> show n

escape :: String -> Latex
escape f = if f `Set.member` latexFunctions then '\\' : f else f

latexFunctions = Set.fromList
    [ "sin"
    , "cos"
    , "tan"
    , "asin"
    , "acos"
    , "atan"
    , "sinh"
    , "cosh"
    , "tanh"
    , "asinh"
    , "acosh"
    , "atanh"
    , "exp"
    , "sqrt"
    , "log"
    ]