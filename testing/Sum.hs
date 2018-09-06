module Sum where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Sum a b = First a | Second b deriving (Eq, Show)

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a,
         return $ Second b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = sumGen

sumGenInts :: Gen (Sum Int Int)
sumGenInts = sumGen

main :: IO ()
main = sample sumGenInts

-- custom probabilities
sumGenFirst :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirst = do
  a <- arbitrary
  b <- arbitrary
  frequency $ [(10, return $ First a), -- First is 10 times more likely to occur
               (1, return $ Second b)]

sumGenIntsFirst :: Gen (Sum Int Int)
sumGenIntsFirst = sumGenFirst

main2 :: IO ()
main2 = sample sumGenIntsFirst
