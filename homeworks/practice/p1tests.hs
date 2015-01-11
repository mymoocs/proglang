{-# OPTIONS_GHC -Wall #-}

module P1tests where
import Test.Hspec
import P1
import Prelude hiding(gcd, lcm)


main :: IO ()
main = hspec $ do
  describe "Practice 1 problems tests" $ do
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
      
          
    
      
    



