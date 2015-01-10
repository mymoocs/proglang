{-# OPTIONS_GHC -Wall #-}

module P1tests where
import Test.Hspec
import P1


main :: IO ()
main = hspec $ do
  describe "Practice 1 problems tests" $ do
    it "Ex 1. isPositive" $ do
      isPositive (1::Int) `shouldBe` True
    it "Ex 2. isDivisibleBy" $ do
      isDivisibleBy 6 3 `shouldBe` True
    



