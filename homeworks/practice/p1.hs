{-# OPTIONS_GHC -Wall #-}

module P1 where
default(Integer)

-- 1.
isPositive :: (Num a, Ord a) => a  -> Bool
isPositive  = (> 0)

-- 2.

isDivisibleBy :: Int -> Int -> Bool
-- isDivisibleBy n k = mod n k == 0
isDivisibleBy n  = (==0) .  mod n


