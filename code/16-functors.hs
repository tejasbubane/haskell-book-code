{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

makeP :: a -> Char
makeP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

makeP' :: [Maybe [Char]] -> Char
makeP' = makeP

liftedP :: [Maybe [Char]] -> [Char]
liftedP = fmap makeP

twiceLiftedP :: [Maybe [Char]] -> [Maybe Char]
twiceLiftedP = (fmap . fmap) makeP

thriceLiftedP :: [Maybe [Char]] -> [Maybe [Char]]
thriceLiftedP = (fmap . fmap . fmap) makeP
-- nothing left to lift any more than this

-- exercises: heavy lifting
a :: [Int]
a = fmap (+1) $ read "[1]"

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c :: Int -> Int
c = fmap (*2) (\x -> x - 2)

d :: Int -> String
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read $ fmap ("123"++) (fmap show ioi)
    in fmap (*3) changed

-- Functors of higher kinded types (more than two type parameters)
data Two' a b =
  Two' a b
  deriving (Eq, Show)

data Or a b =
  First' a
  | Second' b
  deriving (Eq, Show)

-- reduce the types to kind * -> *
instance Functor (Two' a) where
  -- since a is not part of structure, we cannot change it
  fmap f (Two' a b) = Two' a (f b)

instance Functor (Or a) where
  fmap f (First' a) = First' a
  fmap f (Second' b) = Second' (f b)

-- quickCheck properties for functor laws
-- fmap id == id
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity xs = fmap id xs == id xs

-- fmap (f . g) == (fmap f) . (fmap g)
functorCompose' :: (Functor f, Eq (f c)) => f a -> (b -> c) -> (a -> b) -> Bool
functorCompose' xs f g = fmap (f . g) xs == ((fmap f) . (fmap g)) xs

functorCompose :: (Functor f, Eq (f c)) => f a -> Fun b c -> Fun a b -> Bool
functorCompose xs (Fun _ f) (Fun _ g) =
  fmap (f . g) xs == ((fmap f) . (fmap g)) xs

type IntToInt = Fun Int Int
type IntFunctor = IntToInt -> IntToInt -> Bool

-- Instances of Func
-- 1. Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

-- 2. Pair
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return (Pair a a')

-- 3. Two
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- 4. Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

-- 5. Another Three
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return (Three' a b b')

-- 6. Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

-- 7. Another Four
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

-- Exercise: Possibly
data Possibly a =
  LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    oneof $ [return $ LolNope,
             return $ Yeppers a]

-- Exercise: Functor for Sum
data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a)  = First a
  fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof $ [return $ First a
            , return $ Second b]

-- Chapter exercises
-- 1.
data Quant a b =
  Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Desk a
          , return $ Bloor b]

-- 2.
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K a) = K a

instance Arbitrary a => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    return $ K a

-- 3.
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip $ K (f x)

instance Arbitrary b => Arbitrary (Flip K a b) where
  arbitrary = do
    a <- arbitrary
    return $ Flip (K a)

-- 4.
data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    b <- arbitrary
    return $ GoatyConst b

-- not sure how to test ones that follow
-- 5.
data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6.
data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7.
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa ga) = IgnoringSomething fa (fmap f ga)

-- 8.
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9.
data List a =
  Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

-- 10.
data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats ga gb gc) = MoreGoats (fmap f ga) (fmap f gb) (fmap f gc)

-- 11.
data TalkToMe a =
  Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read xs) = Read (fmap f xs)

main = hspec $ do
  describe "Heavy Lifting Functors" $ do
    it "first" $ do
      a `shouldBe` [2]
    it "second" $ do
      b `shouldBe` (Just ["Hi,lol", "Hellolol"])
    it "third" $ do
      (c 1) `shouldBe` (-2)
    it "fourth" $ do
      (d 0) `shouldBe` "1[0,1,2,3]"
    it "fifth" $ do
      v <- e
      v `shouldBe` 3693
  describe "Instances of Func" $ do
    describe "Identity property" $ do
      it "IdentityProp" $ do
        property $ (functorIdentity :: Identity Char -> Bool)
      it "Pair" $ do
        property $ (functorIdentity :: Pair Bool -> Bool)
      it "Two" $ do
        property $ (functorIdentity :: Two String Int -> Bool)
      it "Three" $ do
        property $ (functorIdentity :: Three Int Char Double -> Bool)
      it "Three'" $ do
        property $ (functorIdentity :: Three' Int Bool -> Bool)
      it "Four" $ do
        property $ (functorIdentity :: Four String Int Char Int -> Bool)
      it "Four'" $ do
        property $ (functorIdentity :: Four' Int Char -> Bool)
      it "Possibly" $ do
        property $ (functorIdentity :: Possibly String -> Bool)
      it "Sum" $ do
        property $ (functorIdentity :: Sum String Char -> Bool)
      it "Quant" $ do
        property $ (functorIdentity :: Quant Int Char -> Bool)
      it "K" $ do
        property $ (functorIdentity :: K Int Char -> Bool)
      it "Flip" $ do
        property $ (functorIdentity :: Flip K Int Char -> Bool)
      it "EvilGoateeConst" $ do
        property $ (functorIdentity :: EvilGoateeConst String Int -> Bool)
    describe "Composition property" $ do
      it "Identity" $ do
        property $ (functorCompose :: Identity Int -> IntFunctor)
      it "Pair" $ do
        property $ (functorCompose :: Pair Int -> IntFunctor)
      it "Two Compose" $ do
        property $ (functorCompose :: Two Int Int -> IntFunctor)
      it "Three Compose" $ do
        property $ (functorCompose :: Three Int Int Int -> IntFunctor)
      it "Three'" $ do
        property $ (functorCompose :: Three' Int Int -> IntFunctor)
      it "Four" $ do
        property $ (functorCompose :: Four Int Int Int Int -> IntFunctor)
      it "Four'" $ do
        property $ (functorCompose :: Four' Int Int -> IntFunctor)
      it "Possibly" $ do
        property $ (functorCompose :: Possibly Int -> IntFunctor)
      it "Sum" $ do
        property $ (functorCompose :: Sum Int Int -> IntFunctor)
      it "Quant" $ do
        property $ (functorCompose :: Quant Int Int -> IntFunctor)
      it "K" $ do
        property $ (functorCompose :: K Int Int -> IntFunctor)
      it "Flip" $ do
        property $ (functorCompose :: Flip K Int Int -> IntFunctor)
      it "EvilGoateeConst" $ do
        property $ (functorCompose :: EvilGoateeConst Int Int -> IntFunctor)
