-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b


-- -- Functor Laws
-- -- 1
-- fmap id == id

-- -- 2
-- fmap (f . g) = fmap f . fmap g

-- --3 Structure preservation

data Two a b = Two a b
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

data Three a b c = Three a b c
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
  fmap f (First x) = First x
  fmap f (Second y) = Second (f y)

data Company a b c = DeepBlue a c | Something b
instance Functor (Company k1 k2) where
  fmap f (DeepBlue a c) = DeepBlue a (f c)
  fmap f (Something b) = Something b

data More a b = L b a b | R a b a deriving (Eq, Show)
instance Functor (More a) where
  fmap f (L x y z) = L (f x) y (f z)
  fmap f (R x y z) = R x (f y) z

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)
instance Functor k => Functor (LiftItOut k) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

--- Read - Blog post - "Functors, Applicatives and Monads in Pictures"
