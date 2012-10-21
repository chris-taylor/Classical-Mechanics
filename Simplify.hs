{-# LANGUAGE ViewPatterns #-}

module Simplify where

import Control.Monad (sequence)

import Symbolic

-- |Simplify expressions.
simplify :: (SymbolicExpr a) => a -> a
simplify = collect . expand

-- |Expand out all brackets, distributing multiplication over addition.
expand :: (SymbolicExpr a) => a -> a
expand (sumD  -> Just as)       = sumC (map expand as)
expand (prodD -> Just as)       =
    let sums = map (\(sumD -> Just a) -> a) $ filter isSum as
        rest = map return $ filter (not . isSum) as
    in  sumC $ map prodC $ sequence (rest ++ sums)
expand (divD -> Just (a,b))     = divC (expand a) (expand b)
expand (funD -> Just (str,f,a)) = funC str f (expand a)
expand other                    = other

-- |Collect like terms.
collect :: (SymbolicExpr a) => a -> a
collect other = other