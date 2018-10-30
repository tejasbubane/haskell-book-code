{-# Language InstanceSigs #-}

import Data.ByteString.Lazy hiding (map, putStrLn, foldr, foldMap)
import Network.Wreq
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

-- replace with other websites if desired
urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls

-- Traversable instances
-- Either
data Either' a b =
  Left' a
  | Right' b
  deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Left' x) = Left' x
  fmap f (Right' x) = Right' $ f x

instance Applicative (Either' a) where
  pure = Right'
  Left' e <*> _ = Left' e
  _ <*> Left' e = Left' e
  Right' f <*> Right' e = Right' $ f e

instance Foldable (Either' a) where
  foldMap _ (Left' _) = mempty
  foldMap f (Right' x) = f x

instance Traversable (Either' a) where
  traverse _ (Left' x) = pure (Left' x)
  traverse f (Right' x) = Right' <$> f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Left' a,
           return $ Right' b]

instance (Eq a, Eq b) => EqProp (Either' a b) where
  (=-=) = eq

-- Tuple
data Tuple a b =
  Tuple a b
  deriving (Eq, Show)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a $ f b

instance Monoid a => Applicative (Tuple a) where
  pure = Tuple mempty
  (Tuple a b) <*> (Tuple a' b') = Tuple (a <> a) (b b')

instance Foldable (Tuple a) where
  foldMap f (Tuple _ b) = f b

instance Traversable (Tuple a) where
  traverse f (Tuple a b) = (Tuple a) <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Tuple a b

instance (Eq a, Eq b) => EqProp (Tuple a b) where
  (=-=) = eq

-- Chapter Exercises

-- Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- Constant
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant {getConstant = mempty}
  (Constant a) <*> (Constant a') = Constant (a <> a')

instance Foldable (Constant a) where
  foldMap f (Constant a) = mempty -- not sure why

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a -- no idea why

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant {getConstant = a}

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq

-- Maybe
data Optional a =
  Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    oneof [return Nada,
           return $ Yep a]

instance Eq a => EqProp (Optional a) where (=-=) = eq

-- List
data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a xs) = (f a) <> (foldMap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons a xs) <*> (Cons a' xs') = Cons (a a') (xs <*> xs')

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a xs) = (Cons <$> f a) <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    oneof $ [return Nil,
             return $ pure a]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- Three
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- Theee'
data Three' a b =
  Three' a b b
  deriving (Eq, Ord, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = (f b) <> (f b')

instance Traversable (Three' a) where
  traverse f (Three' a b b') = Three' a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- S
data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = (foldMap f na) <> (f a)

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> (traverse f na) <*> f a

-- Arbitrary and EqProp instances thanks to
-- https://www.reddit.com/r/HaskellBook/comments/57g7lg/ch21_how_to_write_arbitrary_instance_for_s/
instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

-- Instances for Tree
data Tree a =
  Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node left a right) = (foldMap f left) <> (f a) <> (foldMap f right)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node left a right) = Node <$> (traverse f left) <*> (f a) <*> (traverse f right)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    t1 <- arbitrary
    t2 <- arbitrary
    oneof [return $ Empty,
           return $ Leaf a,
           return $ Node t1 a t2]

instance Eq a => EqProp (Tree a) where (=-=) = eq

-- Main
main = do
  putStrLn "Testing Either"
  let eitherTrigger = undefined :: Either' (Int, Int, [Int]) (Int, Int, [Int])
  quickBatch (traversable eitherTrigger)

  putStrLn "Testing Tuple"
  let tupleTrigger = undefined :: Tuple (Int, Int, [Int]) (Int, Int, [Int])
  quickBatch (traversable tupleTrigger)

  putStrLn "Testing Identity"
  let identityTrigger = undefined :: Identity (Int, Int, [Int])
  quickBatch (traversable identityTrigger)

  putStrLn "Testing Constant"
  let constantTrigger = undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int])
  quickBatch (traversable constantTrigger)

  putStrLn "Testing Optional"
  let optionalTrigger = undefined :: Optional (Int, Int, [Int])
  quickBatch (traversable optionalTrigger)

  putStrLn "Testing List"
  let listTrigger = undefined :: List (Int, Int, [Int])
  quickBatch (traversable listTrigger)

  putStrLn "Testing Three"
  let threeTrigger = undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
  quickBatch (traversable threeTrigger)

  putStrLn "Testing Three'"
  let threePrimeTrigger = undefined :: Three' (Int, Int, [Int]) (Int, Int, [Int])
  quickBatch (traversable threePrimeTrigger)

  putStrLn "Testing S"
  let sTrigger = undefined :: S [] (Int, Int, [Int])
  quickBatch (traversable sTrigger)

  putStrLn "Testing Tree"
  let treeTrigger = undefined :: Tree (Int, Int, [Int])
  quickBatch (traversable treeTrigger)
