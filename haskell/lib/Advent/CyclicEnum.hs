module Advent.CyclicEnum (CyclicEnum(..)) where

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred x
        | x == minBound = maxBound
        | otherwise = pred x

    csucc :: a -> a
    csucc x
        | x == maxBound = minBound
        | otherwise = succ x
