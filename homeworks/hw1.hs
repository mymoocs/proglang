-- ProgLang first week homeworks in Haskell
{-# OPTIONS_GHC -Wall #-}
module Hw1 where

data Date = Date Int Int Int  
          deriving Show


numberToString :: Int -> String
numberToString num = show num -- Return a string of the number here!

-- 1.                   
isOlder :: Date -> Date -> Bool
isOlder (Date d1 m1 y1) (Date d2 m2 y2)
  | d1 == d2 && m1 == m2 && y1 == y2 = True
  | d1 <= d2 && m1 <= m2 && y1 <= y2 = True
  | otherwise = False

-- 2.
numberInMonth :: [Date] -> Int -> Int
numberInMonth ds n =
  case ds of
   [] -> 0
   ((Date _ m _) : ds') -> if m == n
                           then 1 + numberInMonth ds' n
                           else  numberInMonth ds' n

-- 3.
numberInMonths :: [Date] -> [Int] -> Int
numberInMonths ds ns =
  case ns of
   [] -> 0
   (n:ns') -> numberInMonth ds n + numberInMonths ds ns'

-- 4.
datesInMonth :: [Date] -> Int -> [Date]
datesInMonth ds m =
  case ds of
   [] -> []
   (d@(Date _ n _) : ds') -> if m == n
                           then d : datesInMonth ds' m
                           else  datesInMonth ds' m
-- 5.   
datesInMonths :: [Date] -> [Int] -> [Date]
datesInMonths ds ms =
  case (ds, ms) of
   ([], _) -> []
   (_, []) -> []
   (ds', (m:ms')) -> datesInMonth ds' m ++ datesInMonths ds' ms'

-- 6.
get_nth :: [String] -> Int -> String
get_nth [] _ = []
get_nth (x:_) 1 = x
get_nth (_:xs) n = get_nth xs (n-1)

-- 7.
date_to_string :: Date -> String
date_to_string (Date d m y) = 
  let ms = ["January","February", "March",
            "April", "May", "June", "July",
            "August", "September", "October",
            "November", "Decembe"]
      month = get_nth ms m
  in month ++ " " ++ (show d) ++ ", " ++ (show y)

-- 8.
number_before_reaching_sum :: Int -> [Int] -> Int
number_before_reaching_sum s xs  =
  if n == length xs
  then   -1
  else   n -- index start from 1
  where 
    fn = (+ (-1)) . length .  takeWhile (< s) . scanl (+) 0
    n = fn xs

-- 9.
what_month :: Int -> Int
what_month n =
  let ms = [31,28,31,30,31,30,31,31,30,31,30,31]
  in number_before_reaching_sum n ms + 1

--- 10.
month_range :: Int -> Int -> [Int]
month_range n m =
  map what_month [n..m]

-- 11.
oldest :: [Date] -> Maybe Date
oldest [] = Nothing
oldest ys =
  Just $ oldest' ys
  where
    oldest' :: [Date] -> Date
    oldest' (x:xs) 
          |null xs = x
          |otherwise = let tailAnswer = oldest' xs
                       in if isOlder x tailAnswer
                          then x
                          else tailAnswer

