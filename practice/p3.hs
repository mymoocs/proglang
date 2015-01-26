{-# OPTIONS_GHC -Wall #-}

module P3 where

import qualified  P1 as P1
import qualified Test.HUnit      as T


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

ex3Fact :: T.Test
ex3Fact = T.TestList
          [
           -- U.teq "factorinal 4" (factorial 4) 24
          ]

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



-- 8. Newton's Method
-- square root is fixed point of 1/2 * (x+n/2)

mySqrt :: Double -> Double
mySqrt = mySqrt' 0.0001

mySqrt' :: (Fractional a, Ord a) => a -> a -> a         
mySqrt' eps n = doUntil f p n
  where
    f = (\x -> 0.5 * (x + n / x))
    p = (\x -> abs(f x - x) > eps)

-- 9. Deeper Into The Woods

data Tree a = Leaf
            | Node {left  :: Tree a
                   ,value  :: a
                   ,right :: Tree a
                   }
            deriving (Show)
                     
treeFold :: (a -> b -> a -> a) ->  a -> Tree b -> a
treeFold _ acc Leaf = acc
treeFold f acc (Node l v r) = 
  let
    lfold = treeFold f acc  l
    rfold = treeFold f acc  r
  in
   f lfold v rfold 

treeI :: Tree Int
treeI = Node Leaf  1 (Node Leaf 2 Leaf)


treeS :: Tree String
treeS = Node (Node Leaf "bar" Leaf)  "foo" (Node Leaf "baz" Leaf)

treeSum :: Tree Integer -> Integer
treeSum = treeFold (\l v r -> l + v +r) 0

treeUnfold :: (a -> Maybe (a, b, a)) -> a -> Tree b
treeUnfold f x = case f x of
                  Nothing        -> Leaf
                  Just (l, v, r) -> Node (treeUnfold f l) v (treeUnfold f r)


--Ex 10. A Grand Challenge

data Expr = LiteralBool
          | LiteralInt
          | BinaryBoolOp Expr Expr
          | BinaryIntOp  Expr Expr
          | Comparison   Expr Expr
          | Conditional  Expr Expr Expr
          deriving (Show)

data ExprType = TypeBool
              | TypeInt
              deriving (Show, Eq)

ex10Expr :: T.Test
ex10Expr = T.TestList
           [
             -- inferType exprI == TypeInt
           ]

exprI :: Expr
exprI = (Conditional LiteralBool
                     LiteralInt
                     (BinaryIntOp LiteralInt LiteralInt))
      

inferTest :: ExprType
inferTest = inferType exprI
           
inferType :: Expr -> ExprType
inferType exp =
  case exp of
   LiteralBool -> TypeBool
   LiteralInt  -> TypeInt
   BinaryBoolOp e1 e2 -> let t1 = inferType e1
                             t2 = inferType e2
                         in
                          if (t1 == t2 && t1 == TypeBool)
                          then t1
                          else error "Type error in BinaryBoolOp"
   BinaryIntOp e1 e2 -> let t1 = inferType e1
                            t2 = inferType e2
                        in
                         if (t1 == t2 && t1 == TypeInt)
                         then  t1
                         else error "Type error in BinaryIntOp"
   Comparison e1 e2 -> let t1 = inferType e1
                           t2 = inferType e2
                       in
                        if (t1 == t2 && t1 == TypeInt)
                        then  TypeBool
                        else error "Type error in Comparison" 
   Conditional e1 e2 e3 ->  let cond = inferType e1
                                t1 = inferType e2
                                t2 = inferType e3
                         in
                             if (cond == TypeBool && t1 == t2)
                             then t1
                             else error "Type error in Conditional" 

  



-- Ex 11. Back To The Future! 2

-- 11.1 GCD - Final Redux
-- for details see p1, ex 6
-- assume that the list is non-empty and
-- all the numbers on the list are positive.
gcdList :: [Int] -> Int
gcdList xs@(x:_) = foldr P1.gcd x xs 

--ToDo: test using P1.gcdList
-- 11.2

anyDivisibleBy :: [Int] -> Int -> Bool
anyDivisibleBy xs d = any (\x -> P1.isDivisibleBy d x) xs
--ToDo: test using P1.
-- 11.3 Quirky Addition { Continued { Final Redux (*)

addAllOpt :: [Maybe Int] -> Maybe Int
addAllOpt = foldr P1.addOpt (Just 0) .  filter (/= Nothing) 
--ToDo: test using P1.addAllOpt

-- 11.4 Flip Flop (*)

-- alternate :: [Int] -> Int
alternate = snd . foldl (\(s, acc) x -> (-s, acc + s * x))  (1,0) 

-- this version give wrong result in even lenght list,
-- because foldr start calcuation from the end of list
alternateR = snd . foldr (\x (s, acc) -> (-s, acc + s * x))  (1,0)
--test using P1.alternate

-- 11.5 Minimum/Maximum - Final Redux (*)
-- test using P1.minMax
minMax :: [Int] -> (Int, Int)
minMax xs@(x:_) = foldr (\x (min', max') ->
                            (if x < min' then x else min',
                             if x > max' then x else max')) (x,x) xs 



-- 12 Lists And Tuples, Oh My! - Final Redux


-- Unit Tests

p3 :: IO T.Counts
p3 = do
    T.runTestTT ex3Fact
    
