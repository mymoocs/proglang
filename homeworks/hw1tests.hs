import Hw1
import Test.QuickCheck
import Test.Hspec

main = hspec $ do
  describe "Testing hw1:" $ do
    it "ex. 1 - isOlder" $ do
      isOlder (Date 1 2 3) (Date 2 3 4) `shouldBe` True
    it "ex. 2 - numberInMonth" $ do
      numberInMonth [Date 1 2 2013,  Date 2 3 2014]  2 `shouldBe` 1
    it "ex. 1 - isOlder" $ do
      isOlder (Date 1 2 3) (Date 2 3 4) `shouldBe` True
    it "ex. 1 - isOlder" $ do
      isOlder (Date 1 2 3) (Date 2 3 4) `shouldBe` True
    it "ex. 1 - isOlder" $ do
      isOlder (Date 1 2 3) (Date 2 3 4) `shouldBe` True
    it "ex. 1 - isOlder" $ do
      isOlder (Date 1 2 3) (Date 2 3 4) `shouldBe` True
    it "ex. 1 - isOlder" $ do
      isOlder (Date 1 2 3) (Date 2 3 4) `shouldBe` True
    it "ex. 1 - isOlder" $ do
      isOlder (Date 1 2 3) (Date 2 3 4) `shouldBe` True
    it "ex. 1 - isOlder" $ do
      isOlder (Date 1 2 3) (Date 2 3 4) `shouldBe` True
    it "ex. 1 - isOlder" $ do
      isOlder (Date 1 2 3) (Date 2 3 4) `shouldBe` True
