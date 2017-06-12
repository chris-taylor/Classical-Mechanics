module HCMUtils.Trio where

data Trio u v w = First u | Second v | Third w deriving (Eq,Ord,Show)

trio :: (u -> a) -> (v -> a) -> (w -> a) -> Trio u v w -> a
trio f _ _ (First a)  = f a
trio _ g _ (Second a) = g a
trio _ _ h (Third a)  = h a
