module Multiplication where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Multiplication" $ do
    it "2 multiplied by 3 is 6" $ do
      (multiply 2 3) `shouldBe` 6
    it "23 multiplied by 3 is 69" $ do
      (multiply 23 3) `shouldBe` 69

multiply :: (Eq a, Num a) => a -> a -> a
multiply num1 num2 = go num1 num1 num2
  where go n1 _ 1 = n1
        go n1 n2 count = go (n1 + n2) n2 (count - 1)
