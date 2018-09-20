import Data.List (elemIndex)
import Data.Monoid
import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Lookups
-- 1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
x' :: Maybe Int
x' = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

-- 4.
xs = [1, 2, 3]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')

-- Exercise: Identity Instance
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity a) (Identity b) = Identity (a b)

-- Exercise: Constant Instance
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

-- This is tricky because Constant can hold on to only one value
-- and that value is part of structure - so we cannot modify it
instance Functor (Constant a) where
  fmap _ (Constant { getConstant = a }) = (Constant { getConstant = a })

instance Monoid a => Applicative (Constant a) where
  pure a = Constant { getConstant = mempty }
  (<*>) (Constant { getConstant = a }) (Constant { getConstant = b }) =
    (Constant { getConstant = (a <> b) })

-- validating user inputs using Maybe Applicative
validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a

-- Explanation of above `mkPerson` code:
-- `Person` first lifted over `Maybe` returned by `mkName`
-- now inside `Maybe` there is a `Person` constructor waiting for
-- second argument `Address` so we have `Maybe` with constructor function
-- and another `Maybe` returned from `mkAddress` - so we use `Applicative`.

-- Another example
data Cow = Cow {
  name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative i =
  if i < 0
  then Nothing
  else Just i

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  Cow <$> noEmpty name'
      <*> noNegative age'
      <*> noNegative weight'
-- liftA3 can also be used
-- liftA3 Cow (noEmpty name')
--            (noNegative age')
--            (noNegative weight')

-- Exercise: Fixer Upper
a :: Maybe String
a = const <$> Just "Hello" <*> pure "World"

b :: Maybe (Int, Int, String, [Int])
b = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- List Applicative Exercise
data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons a xs) <*> (Cons a' xs') = Cons (a a') (xs <*> xs')

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    oneof [return Nil
          , return $ pure a]

instance Eq a => EqProp (List a) where (=-=) = eq

-- ZipList Applicative Exercise
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons a xs) = Cons a (take' (n - 1) xs)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (pure a)
  ZipList' xs <*> ZipList' ys =
    ZipList' (xs <*> ys)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' a

toMyList :: [a] -> List a
toMyList = foldr Cons Nil

-- Exercise: Validations on Either
data Validation e a =
  Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  Success' _ <*> Failure' e  = Failure' e
  Failure' e <*> Success' _  = Failure' e
  Success' a <*> Success' a' = Success' (a a')
  Failure' e <*> Failure' e' = Failure' (e <> e')

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    oneof [return $ Failure' e
          , return $ Success' a]

instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq

-- Chapter exercises
-- 1. Pair
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  Pair a b <*> Pair a' b' =
    Pair (a a') (b b')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance Eq a => EqProp (Pair a) where (=-=) = eq

-- 2. Two
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two a b <*> Two a' b' =
    Two (a <> a') (b b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

-- 3. Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' c' =
    Three (a <> a') (b <> b') (f c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- 4. Three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' a f f' <*> Three' x y z =
    Three' (a <> x) (f y) (f' z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- 5. Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four a b c f <*> Four a' b' c' d =
    Four (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

-- 6. Four'
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' a b c f <*> Four' a' b' c' d =
    Four' (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

-- Combinations
-- generate possible combinations of three input lists using liftA3
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

-- Main
main :: IO ()
main = do
  quickBatch $ applicative (toMyList [('a', 'b', 'c')])
  quickBatch $ applicative (ZipList' $ toMyList [('a', 'b', 'c')])
  quickBatch $ applicative (Success' ("a", "b", "c") :: Validation String (String, String, String))
  -- Chapter Exercises
  quickBatch $ applicative ((Pair (Sum 1, Sum 2, Sum 3) (Sum 4, Sum 5, Sum 6)) ::
                               Pair (Sum Int, Sum Int, Sum Int))
  quickBatch $ applicative (Two (Sum 1, Sum 2, Sum 3) (4, 5, 6) ::
                               Two (Sum Int, Sum Int, Sum Int) (Int, Int, Int))
  quickBatch $ applicative (Three (Sum 1, Sum 2, Sum 3) (Sum 1, Sum 2, Sum 3) (4, 5, 6) ::
                               Three (Sum Int, Sum Int, Sum Int) (Sum Int, Sum Int, Sum Int) (Int, Int, Int))
  quickBatch $ applicative (Three' (Sum 1, Sum 2, Sum 3) (1, 2, 3) (4, 5, 6) ::
                               Three' (Sum Int, Sum Int, Sum Int) (Int, Int, Int))
  quickBatch $ applicative (Four (Sum 1, Sum 2, Sum 3) (Sum 1, Sum 2, Sum 3) (Sum 1, Sum 2, Sum 3) (1, 2, 3) ::
                               Four (Sum Int, Sum Int, Sum Int) (Sum Int, Sum Int, Sum Int) (Sum Int, Sum Int, Sum Int) (Int, Int, Int))
  quickBatch $ applicative (Four' (Sum 1, Sum 2, Sum 3) (Sum 1, Sum 2, Sum 3) (Sum 1, Sum 2, Sum 3) (1, 2, 3) ::
                               Four' (Sum Int, Sum Int, Sum Int) (Int, Int, Int))
