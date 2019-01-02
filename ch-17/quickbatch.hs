-- Detour on using checkers library to test monoid laws
-- Checks has to be installed using `cabal install checkers`

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull =
  Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [(1, return Fools)
              , (1, return Twoo)]

instance Semigroup Bull where
  (<>) _ _ = Fools
  -- this is does not follow identity laws of monoid - hence will fail

instance Monoid Bull where
  mempty = Fools

instance EqProp Bull where (=-=) = eq

-- ZipList Monoid
-- ZipList is defined in Data.Semigroup
instance Semigroup a => Semigroup (ZipList a) where
  a <> b = liftA2 (<>) a b

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty

instance Eq a => EqProp (ZipList a) where (=-=) = eq

main :: IO ()
main = do
  quickBatch $ (monoid Twoo)
  quickBatch $ (monoid (ZipList [1 :: Sum Int]))
