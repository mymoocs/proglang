{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module P2 where

-- 1. 38 Cons Cells (*)

lengthOfaList :: forall a. [a] -> Int
lengthOfaList = len' 0
  where 
    len' :: Int -> [a] -> Int
    len' acc []     = acc
    len' acc (_:xs) = len' (acc + 1) xs 

-- 2. Pass/Fail

type StudentId = Int
type Grade = Int  --  must be in 0 to 100 range

data FinalGrade = FinalGrade
                  { id ::StudentId
                  , grade :: (Maybe Grade)
                  }
                  deriving Show
                  
data PassFail = Pass | Fail deriving (Show, Eq)

-- 2.1 Pass/Fail - 1
passOrFail :: FinalGrade -> PassFail
passOrFail (FinalGrade _ grade) = case grade of
                                   Nothing -> Fail
                                   Just n -> if n >= 75
                                             then Pass
                                             else Fail

-- 2.2 Pass/Fail - 2
hasPassed :: FinalGrade -> Bool
hasPassed grade = case passOrFail grade of
                   Pass -> True
                   _ -> False

-- 2.3 Pass/Fail - 3
numberPassed :: [FinalGrade] -> Int
numberPassed = lengthOfaList . filter (==Pass) . map passOrFail

--2.4 Pass/Fail - 4

groupByOutcome :: [FinalGrade] -> [(PassFail, [StudentId])]
groupByOutcome [] = []
groupByOutcome gs = [(Pass, ps), (Fail, fs)]
  where
    grades = map passOrFail gs
    ps = map (\(FinalGrade n _) -> n) $ filter hasPassed gs
    fs = map (\(FinalGrade n _) -> n) $ filter (not . hasPassed) gs
    

