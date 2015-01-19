{-# OPTIONS_GHC -Wall #-}

module P3tests where

import P3

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "problems tests | Practice 3" $ do
    it "map, filter using fold" $ do
      foldMap (*2) [1..3] `shouldBe` [2, 4, 6::Int]
      foldMap1 (*2) [1..3] `shouldBe` [2, 4, 6::Int]
      foldMap2 (*2) [1..3] `shouldBe` [2, 4, 6::Int]
      foldFilter  even [1..10] `shouldBe` [2, 4, 6::Int, 8, 10]
      foldFilter1 even [1..10] `shouldBe` [2, 4, 6::Int, 8, 10]      
    it "unfold" $ do
      unfold (\x -> if x > 3 then Nothing else  Just (x+1, x)) 0
        `shouldBe` [0, 1, 2, 3::Int]
    it "factorial using unfold and foldl" $ do
      factorial 4 `shouldBe` 24

