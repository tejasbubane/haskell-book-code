module Identity where

import Test.QuickCheck

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

identityGenString :: Gen (Identity String)
identityGenString = identityGen

main :: IO ()
main = do
  sample identityGenInt
  sample identityGenString
