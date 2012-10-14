module Latex where

import Expr

type Latex = String

latex :: Expr -> Latex
latex expr = "\\begin{equation}\n" ++ go expr ++ "\n\\end{equation}\n"
    where
        go (Num n) = show n
        go (Var v) = v
        go (Neg a) = "-" ++ go a
        go (Add a b) = "(" ++ go a ++ " + " ++ go b ++ ")"
        go (Sub a b) = "(" ++ go a ++ " - " ++ go b ++ ")"
        go (Mul a b) = "(" ++ go a ++ " " ++ go b ++ ")"
        go (Div a b) = "\\frac{" ++ go a ++ "}{" ++ go b ++ "}"
        go (Pow a n) = "(" ++ go a ++ ") ^ { " ++ show n ++ "}"
        go (Lit f a) = f ++ "(" ++ go a ++ ")"
        go (App f _ a) = "\\" ++ f ++ "(" ++ go a ++ ")"
