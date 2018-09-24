import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- bind using fmap and join
bind :: Monad m => (a -> m b) -> m a -> m b
bind f xs = join $ fmap f xs

getName :: IO ()
getName = do
  putStr "Enter Name: "
  name <- getLine
  putStrLn ("Hello, " ++ name)

-- same as above but with bind and seq instead of do
getName' :: IO ()
getName' =
  putStr "Enter Name: " >>
  getLine >>=
  \name -> putStrLn ("Hello, " ++ name)

-- List monad in use
-- do syntax isn't just for IO
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs -- pull each value out of List monad (just like List comprehension)
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

-- Maybe monads
data Cow = Cow {
  name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- silly fn: if Cow's name is Bess, weight must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
          else Just c

mkSpericalCow' :: String -> Int -> Int -> Maybe Cow
mkSpericalCow' name' age' weight' = do
  name <- noEmpty name'
  age <- noNegative age'
  weight <- noNegative weight'
  weightCheck (Cow name age weight)
-- computations contributing to the final result can choose to return Nothing
-- based on "previous" computations - Fail Fast
-- eg. if noNegative age' returns Nothing - entire result will be Nothing

-- Either monad
type Founded = Int
type Coders = Int

data SoftwareShop =
  Shop {
      founded :: Founded
    , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError =
  NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

-- Exercise Either Monad
data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  First a <*> _ = First a
  _ <*> First a = First a
  Second f <*> Second a = Second (f a)

instance Monad (Sum a) where
  return = pure
  First a >>= _ = First a
  Second b >>= f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a
          , return $ Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

-- Chapter Exercises
-- 1. Nope
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

-- 2. PhhhbbtttEither
data PhhhbbtttEither b a =
  Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure x = Left' x
  Left' f <*> Left' x = Left' (f x)
  Right' x <*> _ = Right' x
  _ <*> Right' x = Right' x

instance Monad (PhhhbbtttEither b) where
  return = pure
  Left' x >>= f = f x
  Right' x >>= f = Right' x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Left' a
          , return $ Right' b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

-- 3. Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- 4. List
data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f xs <*> Cons x xs' = Cons (f x) (xs <*> xs')

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x Nil >>= f = f x
  Cons x xs >>= f = join $ Cons (f x) (fmap f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ Nil
          , return $ pure a]

instance Eq a => EqProp (List a) where (=-=) = eq

-- Write functions using methods by Monad and Functor
-- just get these to compile
-- 1.
j :: Monad m => m (m a) -> m a
j = (>>= id)

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= (\x -> return $ f x)

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = mb >>= (\b -> (ma >>= (\a -> return $ f a b)))

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a ma f = f <*> ma

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh as f = traverse f as

-- 6.
flipType :: Monad m => [m a] -> m [a]
flipType = traverse id

-- Main
main :: IO ()
main = do
  let triggerSum = undefined :: Sum (Int, Char, Int) (String, Bool, Int)
  quickBatch $ functor triggerSum
  quickBatch $ applicative triggerSum
  quickBatch $ monad triggerSum
  -- Chapter Exercises
  let triggerNope = undefined :: Nope (Int, Bool, Int)
  quickBatch $ functor triggerNope
  quickBatch $ applicative triggerNope
  quickBatch $ monad triggerNope
  let triggerPhhEither = undefined
        :: PhhhbbtttEither (Char, Char, Int) (String, Bool, Char)
  quickBatch $ functor triggerPhhEither
  quickBatch $ applicative triggerPhhEither
  quickBatch $ monad triggerPhhEither
  let triggerList = undefined :: List (Char, Char, Int)
  quickBatch $ functor triggerList
  quickBatch $ applicative triggerList
  quickBatch $ monad triggerList
