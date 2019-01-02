import Data.Monoid
import Data.Semigroup

data Identity a =
  Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z

  foldl f z (Identity x) = f z x

  foldMap f (Identity x) = f x

-- Maybe
data Optional a =
  Nada -- Nothing
  | Yada a -- Just a
  deriving (Eq, Show)

instance Foldable Optional where
  foldr f z Nada = z
  foldr f z (Yada x) = f x z

  foldl f z Nada = z
  foldl f z (Yada x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yada x) = f x

-- Exercises: Write following Library Functions using foldMap or foldr
-- foldMap :: (Data.Monoid m) => (a -> m) -> t a -> m
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
sum :: (Foldable t, Num a) => t a -> a
sum xs = getSum $ Main.foldMap Sum xs

product :: (Foldable t, Num a) => t a -> a
product xs = getProduct $ Main.foldMap Product xs

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = foldr (\x b -> b || a == x) False

minimum :: (Foldable t, Ord a) => t a -> a
minimum = foldr1 min

maximum :: (Foldable t, Ord a) => t a -> a
maximum = foldr1 max

toList :: (Foldable t) => t a -> [a]
toList = foldr (\x b -> x:b) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr (\x b -> x <> b) mempty

-- foldMap using foldr
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (\x b -> f(x) <> b) mempty

-- Chapter Exercises
-- write Foldable instances

-- 1. Constant
data Constant a b =
  Constant a

instance Foldable (Constant a) where
  foldr _ b _ = b

-- 2. Two
data Two a b =
  Two a b

instance Foldable (Two a) where
  foldr f m (Two a b) = (f b m)

-- 3. Three
data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f m (Three _ _ c) = (f c m)

-- 4. Three'
data Three' a b =
  Three' a b b

instance Foldable (Three' a)  where
  foldMap f (Three' a b b') = f b <> f b'

-- 5. Four'
data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = f b <> f b' <> f b''

-- filter using foldMap
filterF :: (Applicative f, Foldable t, Monoid (f a))
  => (a -> Bool) -> t a -> f a
filterF f = Main.foldMap (\a -> if f a then pure a else mempty)
