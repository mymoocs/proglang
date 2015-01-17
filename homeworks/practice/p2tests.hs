{-# OPTIONS_GHC -Wall #-}

module P2tests where
import P2
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "problems test | Practice 2" $ do
    it "Ex 1. 38 Cons Cells (*)" $ do
      lengthOfaList [1..10::Int] `shouldBe` 10
    it "Ex 2.1 Pass/Fail - 1" $ do
     passOrFail (FinalGrade 1 (Just 73)) `shouldBe` Fail
    it "Ex 2.2 Pass/Fail - 2" $ do
     hasPassed (FinalGrade 1 (Just 73)) `shouldBe` False
    it "Ex 2.3 Pass/Fail - 3" $ do
      numberPassed [(FinalGrade 1 (Just 73)), (FinalGrade 1 (Just 77))
                   ,(FinalGrade 1 (Just 55)), (FinalGrade 1 (Just 80))]
        `shouldBe` 2
    it "Ex 2.4 Pass/Fail - 4" $ do
      groupByOutcome [(FinalGrade 21 (Just 73)), (FinalGrade 11 (Just 77))
                   ,(FinalGrade 22 (Just 55)), (FinalGrade 12 (Just 80))]
      `shouldBe` [(Pass,[11, 12]), (Fail, [21, 22])]
  describe "Forest For The Trees section testing" $ do
    it "Ex 3.1 tree height" $ do
      treeHeight tree1 `shouldBe` 3  
    it "Ex 3.2 Sum tree" $ do
      sumTree tree2 `shouldBe` 10
