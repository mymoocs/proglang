{-# OPTIONS_GHC -Wall #-}

module P1tests where
import Test.Hspec
import P1
import Prelude hiding(gcd, lcm, unzip, zip)
-- import Data.Maybe (Maybe)


main :: IO ()
main = hspec $ do
  describe "problems tests | Practice 1" $ do
    it "Ex 1. isPositive" $ do
      isPositive (1::Int) `shouldBe` True
      -- read Defaulting rules articles,
      -- http://www.alexeyshmalko.com/2014/haskell-defaulting-rules/
    it "Ex 2. isDivisibleBy" $ do
      isDivisibleBy 6 3 `shouldBe` True
    it "Ex 3. divideBy" $ do
      divideBy 6 3 `shouldBe` 2
      divideBy 7 3 `shouldBe` 2
    it "Ex 4. Greatest Common Divisor" $ do
      gcd 18 12 `shouldBe` 6
      gcd 7 3 `shouldBe` 1
    it "Ex 5. Least Common Multiple" $ do
      lcm 18 12 `shouldBe` 36
    it "Ex 6. GCD over list" $ do
      gcdList [18, 12, 3] `shouldBe` 3
    it "Ex 7. Element Of A List" $ do
      anyDivisibleBy [13, 1, 20] 5 `shouldBe` True
    it "Ex 8. Save DivideBy" $ do
      safeDivideBy 7 3 `shouldBe` Just 2
      safeDivideBy 7 0 `shouldBe` Nothing

  describe "* problems testing | Practice 1" $ do
    it "Ex 9.  Quirky Addition (*)" $ do
      addOpt (Just 1) (Just 2) `shouldBe` Just 3
      addOpt Nothing (Just 23) `shouldBe` Nothing
    it "Ex 10. Quirky Addition over list (*)" $ do
      addAllOpt [(Just 1), Nothing,  (Just 2)] `shouldBe` Just 3
      addAllOpt [Nothing, (Just 23)] `shouldBe` Just 23
      addAllOpt [] `shouldBe` Nothing
    it "Ex 11. Flip Flop (*)" $ do
      alternate [1,2,3,4] `shouldBe` -2
    it "Ex 12. Minimum/Maximum (*)" $ do
      minMax [1..10] `shouldBe` (1,10)

      
  describe "Lists And Tuples, Oh My! problems testing | Practice 1" $ do
    it "Ex 13(1). unzip" $ do
      unzip [(1, 2), (3, 4), (5, 6)] `shouldBe` ([1, 3, 5], [2, 4,6])
    it "Ex 13(2). zip" $ do
      zip [1, 2, 3]  [4, 6] `shouldBe` [(1, 4), (2, 6)]
    it "Ex 13(3). zipCycle" $ do      
      zipRecycle [1, 2, 3] [4, 6] `shouldBe` [(1, 4), (2, 6),(3, 4)]
    it "Ex 13(4). zipOpt" $ do      
      zipOpt [1, 2, 3] [4, 6] `shouldBe` Nothing
      
