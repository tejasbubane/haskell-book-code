module Division where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      (dividedBy 15 5) `shouldBe` (3, 0)
    it "22 divided by 4 is 5 with remainder 2" $ do
      (dividedBy 22 4) `shouldBe` (5, 2)

-- dividedby using subtraction from previous chapters
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d acc
          | n < d = (acc, n)
          | otherwise = go (n - d) d (acc + 1)
