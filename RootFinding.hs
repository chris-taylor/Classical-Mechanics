{-# LANGUAGE BangPatterns #-}

module RootFinding where

import Debug.Trace

data Root a = NotBracketed
            | SearchFailed
            | Root a
            deriving (Eq,Ord,Show)

instance Functor Root where
    fmap _ NotBracketed = NotBracketed
    fmap _ SearchFailed = SearchFailed
    fmap f (Root a)     = Root (f a)

instance Monad Root where
    return = Root

    NotBracketed >>= _ = NotBracketed
    SearchFailed >>= _ = SearchFailed
    Root a       >>= f = f a

bisectionMethod :: (Fractional a, Ord a, Show a) => a -> (a -> a) -> a -> a -> Root a
bisectionMethod tol f a b
    | fa == 0     = Root a
    | fb == 0     = Root b
    | fa * fb > 0 = NotBracketed -- endpoints have the same sign
    | otherwise   = go 0 a fa b fb
    where
        go !n !a !fa !b !fb
            -- Too many iterations; search failed
            | n > 100   = SearchFailed
            -- Root found; terminate
            | fc == 0   = Root c
            | d < tol   = Root c
            -- Continue iteration
            | otherwise = if signum fc == signum fa
                            then trace (show n ++ ", " ++ show c ++ ", " ++ show b) $ go (n + 1) c fc b fb
                            else trace (show n ++ ", " ++ show a ++ ", " ++ show c) $ go (n + 1) a fa c fc
            where
                !c  = 0.5 * (a + b)
                !fc = f c
                !d  = (b - a) / 2

        !fa = f a
        !fb = f b
