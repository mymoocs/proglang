{-# OPTIONS_GHC -Wall #-}

module P3 where

-- Ex 1 High-Order Fun
-- There Can Be Only One

-- 1.1
foldMap :: (a -> b) -> [a] -> [b]
foldMap f = foldr ((:) . f) []

-- using right fold
foldMap1 :: (a -> b) -> [a] -> [b]
foldMap1 f = foldr (\x xs -> f x : xs) []

-- using left fold
foldMap2 :: (a -> b) -> [a] -> [b]
foldMap2 f = foldl (\xs x -> xs ++ [f x]) []
-- 1.2

--using right fold
foldFilter :: (a -> Bool) -> [a] -> [a]
foldFilter p = foldr (\x xs -> if p x then x:xs else xs) []

-- using left fold
foldFilter1 :: (a -> Bool) -> [a] -> [a]
foldFilter1 p = foldl (\xs x -> if p x then xs ++ [x] else xs) []


-- 2 The Evil Twin
unfold :: (a -> Maybe (a, b)) -> a -> [b]
unfold f x = case f x of
              Nothing -> []
              Just (state, result) -> result : unfold f state

-- 3 A Novel Approach

factorial :: Int -> Int
factorial n = foldl (*) 1 $ unfold (\x -> if x > n
                                          then Nothing
                                          else Just (x+1, x)) 1

-- 4. Unforeseen Developments

unfoldMap :: (a -> b) -> [a] -> [b]
unfoldMap f = unfold (\xs' -> case xs' of
                               [] -> Nothing
                               (x:xs) -> Just $ (xs, f x))
                     
-- 5. So Imperative (*)
doUntil :: (a -> a) -> (a -> Bool) -> a -> a
doUntil f p x = if p x then doUntil f p (f x) else x

-- 6. Yet Another Factorial - Imperative

impFactorial :: Integer -> Integer
impFactorial n = fst $ doUntil (\(acc, r) -> (acc * r, r-1)) (\(_ , r) -> r > 0)
                 (1, n)

-- 7. Fixed Point (*)
fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f = doUntil f (\x -> f x /= x) 




mySqrt :: Double -> Double
mySqrt = mySqrt' 0.0001

mySqrt' :: (Fractional a, Ord a) => a -> a -> a         
mySqrt' eps n = doUntil f p n
  where
    f = (\x -> 0.5 * (x + n / x))
    p = (\x -> abs(f x - x) > eps)

    
-- 8. Newton's Method
-- square root is fixed point of 1/2 * (x+n/2)