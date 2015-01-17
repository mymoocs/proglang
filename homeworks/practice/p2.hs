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

-- 3. Forest For The Trees

-- 3.1 Tree height
data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving Show

data TreeR a =LeafR | NodeR { left :: TreeR a
                            , value :: a
                            , right:: TreeR a 
                            }
             deriving Show

tree :: Tree Int
tree = Node Leaf  1 (Node Leaf 2 Leaf)

treeR :: TreeR Int
treeR = NodeR LeafR  1 (NodeR LeafR 2 LeafR)

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node l _ r) = 1 + max lheight rheight
  where
    lheight = treeHeight l
    rheight = treeHeight r    

tree1 :: Tree Int
tree1 = Node (Node (Node Leaf 0 Leaf) 0 (Node Leaf 0 Leaf ))
           0 (Node Leaf 0 Leaf)
        
-- 3.2 Sum tree
sumTree :: Tree Int -> Int
sumTree Leaf = 0
sumTree (Node l v r) = v + sumTree l + sumTree r

tree2 :: Tree Int
tree2 = Node (Node (Node Leaf 3 Leaf) 2 Leaf)
           1 (Node Leaf 4 Leaf)


-- 3.3 Gardener
data Flag = LeaveMeAlone
          | PruneMe
          deriving (Show, Eq)

gardener :: Tree Flag -> Tree Flag
gardener Leaf = Leaf
gardener (Node l v r) |v==PruneMe = Leaf
                      |otherwise = Node (gardener l) v (gardener r)

tree3 :: Tree Flag
tree3 = Node (Node (Node Leaf LeaveMeAlone Leaf) PruneMe Leaf)
           LeaveMeAlone (Node Leaf LeaveMeAlone Leaf)


-- Back To The Future!
{-
 example of this sample suggest to do several problems from p1
 using pattern matching not list function, my solutions all use
 pattern matching trick, since laready done.
-}
