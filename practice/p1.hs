{-# OPTIONS_GHC -Wall #-}

module P1 ( gcd
          , gcdList
          , isDivisibleBy
          , anyDivisibleBy
          , addOpt
          , addAllOpt
          , alternate
          , minMax
          ) where


import  Prelude hiding(gcd, unzip, zip)
-- import Data.Maybe
default(Integer)

-- 1. Positive Numbers
isPositive :: (Num a, Ord a) => a  -> Bool
isPositive  = (> 0)

-- 2. Divisibility
-- assume that the second argument will be non-zero.
isDivisibleBy :: Int -> Int -> Bool
-- isDivisibleBy n k = mod n k == 0
isDivisibleBy n  = (==0) .  mod n

-- 3. Integer Division
-- assume that the 1st argument is non-negative
-- and the second one is strictly positive.
divideBy :: Int -> Int -> Int
divideBy n d   | n < d = 0
               |otherwise = 1 + divideBy (n-d) d

-- 4. Greatest Common Divisor
-- assume that both numbers are positive.
gcd :: Int -> Int -> Int
gcd n m |n==m = n
        |n>m = gcd (n-m) m
        |otherwise = gcd n (m-n)
    

-- 5. Least Common Multiple
-- assume that both numbers are positive.

lcm :: Int -> Int -> Int
lcm n m = n * (m `div` gcd n m)



-- 6. Greatest Common Divisor { Continued
-- assume that the list is non-empty and
-- all the numbers on the list are positive.
gcdList :: [Int] -> Int
gcdList [x] = x
gcdList (x:xs) = gcd x (gcdList xs)

-- 7. Element Of A List
anyDivisibleBy :: [Int] -> Int -> Bool
anyDivisibleBy [] _ = False
anyDivisibleBy (x:xs) d |isDivisibleBy d x = True
                        |otherwise = anyDivisibleBy xs d
--anyDivisibleBy xs d =  any (isDivisibleBy d) xs

-- 8. Integer Division - Continued

safeDivideBy :: Int -> Int -> Maybe Int
safeDivideBy _ 0 = Nothing
safeDivideBy n d = Just $ n `div` d


-- * problems

-- 9. Quirky Addition (*)
addOpt :: Maybe Int -> Maybe Int -> Maybe Int
addOpt Nothing _ = Nothing
addOpt _ Nothing = Nothing
addOpt (Just x) (Just y) =Just $  x + y


-- 10. Quirky Addition (*) - continued
addAllOpt :: [Maybe Int] -> Maybe Int
addAllOpt xs |null xs = Nothing
             |otherwise = addAllOpt' xs
  where
       addAllOpt' :: [Maybe Int] -> Maybe Int
       addAllOpt' [] = Just 0
       addAllOpt' (m:ms) =
         case m of
         Nothing  -> addAllOpt' ms
         _        -> addOpt m (addAllOpt' ms)

-- 11. Flip Flop (*)

alternate :: [Int] -> Int
alternate [] = 0
alternate [x] = x
alternate (x:y:xs) = x + (-y) + alternate xs

-- 12. Minimum/Maximum (*)
--  assume that the list is non-empty and
minMax :: [Int] -> (Int, Int)
minMax (n:ns) = minMax' n n ns
  where
    minMax' :: Int -> Int -> [Int] -> (Int,Int)
    minMax' m1 m2  [] = (m1, m2) 
    minMax' m1 m2 (x:xs) = let m1' = if m1 > x then x else m1
                               m2' = if m2 < x then x else m2
                           in minMax' m1' m2' xs

-- 13. Lists And Tuples, Oh My!
unzip :: [(Int,Int)] -> ([Int], [Int])
unzip [] = ([], [])
unzip ps  = ( [a | (a,_) <- ps], [b | (_,b) <- ps]) 

-- 14. Lists And Tuples, Oh My! -- Continued (1)
zip :: [Int] -> [Int] -> [(Int, Int)] 
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- 15. Lists And Tuples, Oh My! -- Continued (2)
zipRecycle :: [Int] -> [Int] -> [(Int,Int)]
zipRecycle xs ys = let n = length xs
                       m = length ys
                   in  zip xs ys ++ if n > m
                                    then zip (drop m xs) ys
                                    else zip xs (drop n ys)


-- 16. Lists And Tuples, Oh My! -- Continued (3)
zipOpt :: [Int] -> [Int] -> Maybe [(Int, Int)] 
zipOpt xs ys |length xs == length ys = Just $ zip xs ys
             |otherwise = Nothing

-- 17. BananaBanana
duplicate :: [String] -> [String]
duplicate [] = []
duplicate (x:xs) = x:x:duplicate xs

-- 18. Greetings, Earthlings! (*)
greeting :: Maybe String -> String
greeting name = let you = case name of
                           Nothing -> "..."
                           Just str -> str
                in "Hello there, " ++ you ++ "!"

-- 19. BananaBanana - Continued
-- assume that the second argument is non-negative.
repeats :: String -> Int -> [String]
repeats s 0 = [s]
repeats str n = repeats' str n 1
  where
    rep :: String -> Int -> String
    rep _ 0 = []
    rep s m = s ++ rep s (m-1)

    repeats' :: String -> Int -> Int -> [String]
    repeats' _ 0 _ = []
    repeats' s q k = rep s k : repeats' s (q-1) (k+1)


-- 20. BananaBanana { Continued (Again) (*)
-- ssume that both lists have the same length.

repeatsList :: [String] -> [Int] -> [String]
repeatsList [] _ = []
repeatsList (s:ss) (n:ns) = cycle s n ++ repeatsList ss ns
  where
    cycle :: String -> Int -> [String]
    cycle _ 0    = []
    cycle item m = item : cycle item (m-1)
