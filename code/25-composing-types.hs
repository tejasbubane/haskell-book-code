{-# LANGUAGE InstanceSigs #-}

newtype Compose f g a =
  Compose { getCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure (pure a)

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose fgab) <*> (Compose fga) =
    Compose $ ((<*>) <$> fgab <*> fga)

-- Exercises: Compose Instances

-- Write foldable instance for Compose
instance (Foldable f, Foldable g) =>
         Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) =>
         Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

-- Sidetrack: Noting to do with chapter
class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- Its a functor that can map over two type arguments instead of just one.
-- Write binfunctor instances for following types.

-- 1.
data Deux a b = Deux a b
instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

-- 2.
data Const a b = Const a
instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

-- 3.
data Drei a b c = Drei a b c
instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 4.
data SuperDrei a b c = SuperDrei a b
instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

-- 5.
data SemiDrei a b c = SemiDrei a
instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

-- 6.
data Quadriceps a b c d = Quadzzz a b c d
instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

-- 7.
data Either' a b = Left' a | Right' b
instance Bifunctor Either' where
  bimap f _ (Left' a) = Left' (f a)
  bimap _ g (Right' b) = Right' (g b)

-- Identity Monad Transformer
newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f
