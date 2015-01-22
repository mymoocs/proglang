{-# OPTIONS_GHC -Wall #-}

module P3tests (main) where

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
    it "Ex 2. unfold" $ do
      unfold (\x -> if x > 3 then Nothing else  Just (x+1, x)) 0
        `shouldBe` [0, 1, 2, 3::Int]
    it "Ex 3. factorial using unfold and foldl" $ do
      factorial 4 `shouldBe` 24

    it "Ex 4. map with unfold" $ do
      unfoldMap (+1) [1..3] `shouldBe` [2,3,4::Int]

    it "Ex 5. doUntil" $ do
      doUntil (\x -> x+1) (<10) 1 `shouldBe` 10

    it "Ex 6. Imperative factorial using doUntil" $ do
      factorial 4 `shouldBe` 24

    it "Ex 7. sqrt by Newton's method(fixed point)" $ do
      abs(mySqrt 2 - sqrt 2) < 0.01 `shouldBe` True

    describe "problems on Tree | practice 3" $ do
      it "Ex 8. fold tree" $ do
        treeFold (\l v r -> l + v + r) 0 treeI `shouldBe` 3
        treeFold (\l v r -> l ++ v ++ r) "!" treeS `shouldBe` "!bar!foo!baz!"
  
