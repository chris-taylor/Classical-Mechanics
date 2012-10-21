{-# LANGUAGE ViewPatterns #-}

-- |Classes for symbolic algebra. This is a simplified version of the symbolic
--algebra code in Algebra.HaskSymb, available on github at
--https://github.com/colah/HaskSymb/
--
--This module defines classes which provide constructors (postfixed C) and
--destructors (postfixed D) for algebraic concepts. Each algebraic concept
--has its own class.

module Symbolic where

import Prelude hiding (Real)

type Real = Double
type Var  = String

-- |Instances of the 'Symbolic' class denotes types that have both constants
--and variables.
class Symbolic a where
    constC :: Real -> a
    constD :: a -> Maybe Real
    varC :: Var -> a
    varD :: a -> Maybe Var

-- |Instances of the 'SymbolicSum' class are types that can be summed.
class SymbolicSum a where
    sumC :: [a] -> a
    sumD ::  a  -> Maybe [a]

-- |Instances of the 'SymbolicProd' class are types that can be multiplied.
class SymbolicProd a where
    prodC :: [a] -> a
    prodD ::  a -> Maybe [a]

-- |Instances of the 'SymbolicDiv' class are types that can be divided.
class SymbolicDiv a where
    divC :: a -> a -> a
    divD :: a -> Maybe (a,a)

-- |Instances of the 'SymbolicRealFun' class have floating point functions.
class SymbolicRealFun a where
    funC :: String -> (Real -> Real) -> a -> a
    funD :: a -> Maybe (String, Real -> Real, a)

-- |Instances of the 'SymbolicLiteralFun' class have literal functions.
class SymbolicLiteralFun a where
    litC :: String -> a -> a
    litD :: a -> Maybe (String, a)

-- |Convenience function - is its argument a constant?
isConst :: Symbolic a => a -> Bool
isConst v = case constD v of
                Just _  -> True
                Nothing -> False

-- |Symbolically sum the values in a list, pulling constants to the front
--and recursively expanding out nested sums.
sumC' :: (Symbolic a, SymbolicSum a) => [a] -> a
sumC' vals =
    let sums      = filter isSum vals
        nonSums   = filter (not . isSum) vals
        consts    = map (\(constD -> Just a) -> a) (filter isConst vals)
        nonConsts = filter (not . isConst) vals

        isSum v   = case sumD v of
                        Just _  -> True
                        Nothing -> False

        sumC'' [x] = x
        sumC'' xs  = sumC xs

    in  if null sums

            then if null consts
                    then sumC'' vals
                    else sumC'' $
                            (if sum consts == 0
                                then []
                                else [constC (sum consts)]) ++ nonConsts

            else sumC' $ concatMap (\(sumD -> Just a) -> a) sums ++ nonSums

-- |Symbolically multiply the values in a list, pulling constants to the front
--and recursively expanding out nested products.
prodC' :: (Symbolic a, SymbolicProd a) => [a] -> a
prodC' vals =
    let prods     = filter isProd vals
        nonProds  = filter (not . isProd) vals
        consts    = map (\(constD -> Just a) -> a) (filter isConst vals)
        nonConsts = filter (not . isConst) vals

        isProd v  = case prodD v of
                        Just _  -> True
                        Nothing -> False

        prodC'' [x] = x
        prodC'' xs  = prodC xs

    in  if null prods

            then if null consts
                then prodC'' vals
                else case (nonConsts, product consts) of
                        ([],n) -> constC n
                        (_, 0) -> constC 0
                        (l, 1) -> prodC l
                        (l, n) -> prodC $ (constC n) : l

            else prodC' $ concatMap (\(prodD -> Just a) -> a) prods ++ nonProds

-- |Equality between two lists, ignoring ordering. This is O(n^2) in the length
--of the list.
listEq :: (a -> a -> Bool) -> [a] -> [a] -> Bool
listEq eqfn (a:as) bs =
    let matches    = filter (`eqfn` a) bs
        nonmatches = filter (not . (`eqfn` a)) bs

    in  if null matches
            then False
            else listEq eqfn as (tail matches ++ nonmatches)

listEq _ [] [] = True
listEq _  _  _ = False

-- |Equality for symbolic expressions.
(===) :: (Symbolic a, SymbolicSum a, SymbolicProd a,
          SymbolicDiv a, SymbolicRealFun a) => a -> a -> Bool
(constD -> Just a)     === (constD -> Just b)     = a == b
(varD   -> Just a)     === (varD   -> Just b)     = a == b
(sumD   -> Just a)     === (sumD   -> Just b)     = listEq (===) a b
(prodD  -> Just a)     === (prodD  -> Just b)     = listEq (===) a b
(divD -> Just (a,c))   === (divD -> Just (b,d))   = a === b && c === d
(funD -> Just (f,_,a)) === (funD -> Just (g,_,b)) = f == g && a === b
_                      === _                      = False




