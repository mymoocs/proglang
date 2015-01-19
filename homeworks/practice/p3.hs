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
factorial n = foldl (*) 1 $ unfold (\x -> if x > n then Nothing else Just (x+1, x)) 1
