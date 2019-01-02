module Trivial where

import Test.QuickCheck

data Trivial = Trivial deriving (Show, Eq)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

main :: IO ()
main = sample trivialGen
