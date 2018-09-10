-- Monoid instance for Maybe type renamed to Optional

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada Nada         = Nada
  (<>) s@(Only _) Nada   = s
  (<>) Nada s@(Only _)   = s
  (<>) (Only x) (Only y) = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty                    = Nada

optionalGen :: (Arbitrary a) => Gen (Optional a)
optionalGen = do
  a <- arbitrary
  oneof [return Nada
        , return $ Only a]

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = optionalGen

-- testing properties of Monoids
prop_assoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
prop_assoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

prop_leftIdentity :: (Eq m, Monoid m) => m -> Bool
prop_leftIdentity a = (mempty <> a) == a

prop_rightIdentity :: (Eq m, Monoid m) => m -> Bool
prop_rightIdentity a = (a <> mempty) == a

-- Maybe Another Monoid
newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup a => Semigroup (First' a) where
  (<>) (First' { getFirst' = x }) (First' { getFirst' = y }) =
    First' { getFirst' = (x <> y) }

instance Semigroup a => Monoid (First' a) where
  mempty = First' { getFirst' = Nada }

firstMappend :: Semigroup a => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- arbitrary
  return $ First' { getFirst' = a }

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

-- NonEmpty is example of datatype that can have instance of Semigroup but not Monoid
data NonEmpty a = a :| [a]
  deriving (Eq, Ord, Show)
-- no identity value by design

type NonEmptyMappend =
  NonEmpty String -> NonEmpty String -> NonEmpty String -> Bool

instance Semigroup a => Semigroup (NonEmpty a) where
  (<>) (x :| xs) (y :| ys) = (x <> y) :| (xs <> ys)

nonEmptyGen :: Arbitrary a => Gen (NonEmpty a)
nonEmptyGen = do
  a <- arbitrary
  return $ a :| []

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = nonEmptyGen

-- Semigroup Exercises
-- 1. Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialMappend = Trivial -> Trivial -> Trivial -> Bool

-- 2. Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity $ x <> y

identityGen :: (Arbitrary a) => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return $ Identity a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

type IdentityMappend = Identity String -> Identity String -> Identity String -> Bool

-- 3. Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoMap = Two String [Int]
type TwoMappend = TwoMap -> TwoMap -> TwoMap -> Bool

-- 4. Three
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

type ThreeMap = Three String [Int] [String]
type ThreeMappend =
  ThreeMap -> ThreeMap -> ThreeMap -> Bool

-- 5. Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') =
    Four (a <> a') (b <> b') (c <> c') (d <> d')

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = fourGen

type FourMap = Four String [Int] [Bool] [Double]
type FourMappend = FourMap -> FourMap -> FourMap -> Bool

-- 6. BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

boolConjGen :: Gen BoolConj
boolConjGen = do
  a <- arbitrary
  return $ BoolConj a

instance Arbitrary BoolConj where
  arbitrary = boolConjGen

type BoolConjMappend = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7. BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

boolDisjGen :: Gen BoolDisj
boolDisjGen = do
  a <- arbitrary
  return $ BoolDisj a

instance Arbitrary BoolDisj where
  arbitrary = boolDisjGen

type BoolDisjMappend = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8. Or
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (Fst a) <> (Fst a') = Fst (a <> a')
  (Snd b) <> _        = Snd b
  _ <> (Snd b)        = Snd b

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  oneof $ [return $ Fst a
          , return $ Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

type OrMap = Or (Sum Int) String
type OrMappend = OrMap -> OrMap -> OrMap -> Bool

-- 9. Combine
newtype Combine a b =
  Combine { unCombine :: (a -> b) } -- This is confusing


-- 10.
newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (Comp { unComp = f }) <> (Comp { unComp = f' }) =
    Comp { unComp = f . f' } -- Not sure how to write arbitrary instance for this

-- 11. Validation
data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Main.Failure a) <> (Main.Failure a') = Main.Failure (a <> a')
  _ <> (Main.Success b) = Main.Success b
  (Main.Success b) <> _ = Main.Success b

validationGen :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
validationGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Main.Failure a
        , return $  Main.Success b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = validationGen

type ValidationMap = Validation [Int] [Int]
type ValidationMappend = ValidationMap -> ValidationMap -> ValidationMap -> Bool

-- 12. AccumulateRight
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Main.Success b)) <> (AccumulateRight (Main.Success b')) =
    AccumulateRight (Main.Success (b <> b'))
  (AccumulateRight (Main.Failure a)) <> _ = AccumulateRight (Main.Failure a)
  _ <> (AccumulateRight (Main.Failure a)) = AccumulateRight (Main.Failure a)

acRightGen :: (Arbitrary a, Arbitrary b) => Gen (AccumulateRight a b)
acRightGen = do
  a <- validationGen
  return (AccumulateRight a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = acRightGen

type ACRightMap = AccumulateRight [Int] [Int]
type ACRightMappend = ACRightMap -> ACRightMap -> ACRightMap -> Bool

-- 13. AcuumulateBoth

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Main.Success b)) <> (AccumulateBoth (Main.Success b')) =
    AccumulateBoth (Main.Success (b <> b'))
  (AccumulateBoth (Main.Failure a)) <> (AccumulateBoth (Main.Failure a')) =
    AccumulateBoth (Main.Failure (a <> a'))
  (AccumulateBoth (Main.Failure a)) <> _ = AccumulateBoth (Main.Failure a)
  _ <> (AccumulateBoth (Main.Failure a)) = AccumulateBoth (Main.Failure a)

accBothGen :: (Arbitrary a, Arbitrary b) => Gen (AccumulateBoth a b)
accBothGen = do
  a <- validationGen
  return (AccumulateBoth a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = accBothGen

type AccBothMap = AccumulateBoth [Char] [Int]
type AccBothMappend = AccBothMap -> AccBothMap -> AccBothMap -> Bool

-- Monoid Exercises
-- For all datatypes defined above - hence commented below
-- since Semigroup instances are already defined, with mappend (<>)
-- All that is needed for Monoids is to find the identity (mempty)
-- And we can test left and right associativity

-- 1. Trivial
-- data Trivial = Trivial deriving (Eq, Show)
instance Monoid Trivial where
  mempty = Trivial

-- 2. Identity
-- newtype Identity a = Identity a deriving (Eq, Show)
instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

-- 3. Two
-- data Two a b = Two a b deriving (Eq, Show)
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

-- 4. BoolConj
-- newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Monoid BoolConj where
  mempty = BoolConj True

-- 5. BoolDisj
-- newtype BoolDist = BoolDisj Bool deriving (Eq, Show)
instance Monoid BoolDisj where
  mempty = BoolDisj False

-- 6. Combine
-- newtype Combine a b = Combine { unCombine :: (a -> b) }
-- Not sure how to do this one

-- 7. Comp
-- newtype Comp a = Comp (a -> a)
instance Monoid (Comp a) where
  mempty = Comp $ id
-- Not sure how to test this one

-- 8. Skipped

-- Main

main :: IO ()
main = do
  -- Study
  quickCheck (prop_assoc :: String -> String -> String -> Bool)
  quickCheck (prop_leftIdentity :: String -> Bool)
  quickCheck (prop_rightIdentity :: String -> Bool)
  quickCheck (prop_assoc :: FirstMappend)
  quickCheck (prop_leftIdentity :: FstId)
  quickCheck (prop_rightIdentity :: FstId)
  quickCheck (prop_assoc :: NonEmptyMappend)

  -- Semigroup exercises tests
  quickCheck (prop_assoc :: TrivialMappend)
  quickCheck (prop_assoc :: IdentityMappend)
  quickCheck (prop_assoc :: TwoMappend)
  quickCheck (prop_assoc :: ThreeMappend)
  quickCheck (prop_assoc :: FourMappend)
  quickCheck (prop_assoc :: BoolConjMappend)
  quickCheck (prop_assoc :: BoolDisjMappend)
  quickCheck (prop_assoc :: OrMappend)
  quickCheck (prop_assoc :: ValidationMappend)
  quickCheck (prop_assoc :: ACRightMappend)
  quickCheck (prop_assoc :: AccBothMappend)

  -- Monoid Exercises tests
  quickCheck (prop_leftIdentity :: Trivial -> Bool)
  quickCheck (prop_rightIdentity :: Trivial -> Bool)
  quickCheck (prop_leftIdentity :: Identity (Sum Int) -> Bool)
  quickCheck (prop_rightIdentity :: Identity String -> Bool)
  quickCheck (prop_leftIdentity :: Two (Product Int) String -> Bool)
  quickCheck (prop_rightIdentity :: Two (Product Int) String -> Bool)
  quickCheck (prop_leftIdentity :: BoolConj -> Bool)
  quickCheck (prop_rightIdentity :: BoolConj -> Bool)
  quickCheck (prop_leftIdentity :: BoolDisj -> Bool)
  quickCheck (prop_rightIdentity :: BoolDisj -> Bool)
