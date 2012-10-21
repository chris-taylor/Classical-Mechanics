{-# LANGUAGE ViewPatterns #-}

module Simplify where

import Control.Monad (sequence)

import Expr
import Symbolic

-- |Expand out all brackets, distributing multiplication over addition.
expand :: (SymbolicSum a, SymbolicProd a, SymbolicDiv a) => a -> a
expand (sumD  -> Just as)   = sumC (map expand as)
expand (prodD -> Just as)   =
    let sums = map (\(sumD -> Just a) -> a) $ filter isSum as
        rest = map return $ filter (not . isSum) as

    in  sumC $ map prodC $ sequence (rest ++ sums)
expand (divD -> Just (a,b)) = divC (expand a) (expand b)
expand other                = other

