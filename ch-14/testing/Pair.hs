module Pair where

import Test.QuickCheck

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Pair a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

arbPairIntInt :: Gen (Pair Int Int)
arbPairIntInt = pairGen

arbPairIntString :: Gen (Pair Int String)
arbPairIntString = pairGen

main :: IO ()
main = do
  sample arbPairIntInt
  sample arbPairIntString
