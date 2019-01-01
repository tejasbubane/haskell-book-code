{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- IdentityT Transformer
newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }

-- Instances for IdentityT defined in previous chapter but one example below
-- requires Monad instance, so instead of importing lets define again here
instance Functor f => Functor (IdentityT f) where
  fmap :: (a -> b) -> IdentityT f a -> IdentityT f b
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative f => Applicative (IdentityT f) where
  pure :: a -> IdentityT f a
  pure x = IdentityT $ pure x

  (<*>) :: IdentityT f (a -> b) -> IdentityT f a -> IdentityT f b
  (IdentityT fab) <*> (IdentityT fa) = IdentityT $ fab <*> fa

instance Monad f => Monad (IdentityT f) where
  return :: a -> IdentityT f a
  return = pure

  (>>=) :: (IdentityT f a) -> (a -> IdentityT f b) -> IdentityT f b
  (IdentityT fa) >>= f =
    IdentityT $ do
      a <- fa
      runIdentityT $ f a

-- MaybeT Transformer
newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

-- Nothing special required for Functors
-- Difference is for Monad instances only - rest is all same
instance (Functor m) => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

instance (Applicative m) => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure x = MaybeT $ pure (pure x)

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT mmab) <*> (MaybeT mma) =
    MaybeT $ (<*>) <$> mmab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing  -> return Nothing
        (Just x) -> runMaybeT (f x)


-- EitherT Transformer
newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }
-- Note the type arguments as `e m a` - this is to
-- write Functor and Applicatives over `a`

-- Exercises:
-- 1. Functor instance
instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

-- 2. Applicative Instance
instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure x = EitherT (pure (pure x))

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT meab) <*> (EitherT mea) =
    EitherT ((<*>) <$> meab <*> mea)

-- 3. Monad Instance
instance Monad m => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea) >>= f =
    EitherT $ do
      v <- mea
      case v of
        Left err -> return (Left err)
        Right a  -> runEitherT (f a)

-- 4. swapEitherT helper function
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

-- Hint was given to write above in terms of swapEither
-- swapEither is nothing but swapping left and right of an either
swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a

-- 5. Transformer variant of the either catamorphism
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT meab) =
  join $ (either fa fb) <$> meab


-- ReaderT Transformer
newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rma) = ReaderT $ \r -> f <$> rma r

instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure x = ReaderT $ \r -> pure x

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT rmab) <*> (ReaderT rma) =
    ReaderT $ \r -> (rmab r) <*> (rma r)

instance Monad m => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      v <- rma r
      (runReaderT $ f v) r

-- StateT Transformer
newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT smas) =
    StateT $ \s -> (\(a,s) -> (f a, s)) <$> (smas s)

-- The book says there should be Monad m constraint here
-- but I was able to write the instance using Applicative ??
instance Applicative m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> pure (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smabs) <*> (StateT smas) =
    StateT $ \s -> (\(ab, _) (a, _) -> (ab a, s)) <$> (smabs s) <*> (smas s)

instance Monad m => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f =
    StateT $ \s -> do
      (a, s') <- sma s
      (runStateT (f a) s')

-- Exercise: Wrap it Up
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT . ExceptT . ReaderT $ const (return (Right (Just 1)))

-- MonadTrans
instance MonadTrans IdentityT where
  lift :: Monad m => m a -> IdentityT m a
  lift = IdentityT

-- Exercises: MonadTrans Instances
instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (ReaderT r) where
  lift ma = ReaderT $ \r -> ma

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> liftM (flip (,) s) ma
-- worked this^ one out using typed holes! Super proud! :power:

-- MonadIO
instance MonadIO m => MonadIO (IdentityT m) where
  liftIO = IdentityT . liftIO

instance MonadIO m => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

-- Exercises MonadIO Instances
instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO
