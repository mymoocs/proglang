-- ProgLang first week homeworks in Haskell

module DateProcessing where

data Date = Date Int Int Int  -- Int -> Int -> Int
          deriving Show

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
   (ds, (m:ms')) -> datesInMonth ds m ++ datesInMonths ds ms'
  
{-
get_nth = fn : string list * int -> string
date_to_string = fn : int * int * int -> string
number_before_reaching_sum = fn : int * int list -> int
what_month = fn : int -> int
month_range = fn : int * int -> int list
oldest = fn : (int * int * int) list -> (int * int * int) option  
-}
