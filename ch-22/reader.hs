{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Data.Char
import Control.Monad
import Data.Maybe

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

-- same as bip -- Functor of functions
bloop :: Integer -> Integer
bloop = fmap boop doop

-- Applicative of functions
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

-- Monad for functions
boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return $ a + b

-- short exercise: warming up
cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

monadic :: [Char] -> ([Char], [Char])
monadic = do
  a <- rev
  b <- cap
  return (a, b)

monadicBind :: [Char] -> ([Char], [Char])
monadicBind = cap >>= \a -> rev >>= \b -> return (a, b)

newtype Reader r a = Reader { runReader :: r -> a }

-- Exercise: Ask
ask :: Reader a a
ask = Reader id

-- Exercise: Reading Comprehension
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

-- Implement Applicative for Reader
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \r -> f (ra r)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

-- Exercise: Reader Monad

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = join $ Reader $ \r -> aRb (ra r)

-- Warmup stretch
x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequA 7
  print $ foldr (&&) True $ sequA 7
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
