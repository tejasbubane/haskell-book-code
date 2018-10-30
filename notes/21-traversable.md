# Traversable

* Traversable allows to transform elements inside structure like a functor
  producing applicative effects along the way and lift those potentially
  multiple insance of Applicative structure outside of Traversable.

```haskell
class (Functor t, Foldable t) => Traversable t where
  {-# MINIMAL traverse | sequenceA #-}
  traverse :: Applicative f =>
              (a -> f b)
           -> t a
           -> f (t b)
  traverse f = sequenceA . fmap f

  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id
```

* `sequence` flips the contexts/structures around - `t (f a) -> f (t b)`

* `traverse` is just `fmap` and then `sequence` where `sequence` is the unique bit.

* This is similar to `>>=` which is `fmap` and then `join` where join is
  unique to monoid.

#### Traversable Laws

- For `traverse` function:

* **Naturality**:

```haskell
t . traverse f = traverse (t . f)
```

* **Identity**:

```haskell
traverse Identity = Identity
```
cannot add or inject any structure or `effects` - just flip.

* **Composition**:

```haskell
traverse (Compose . fmap g . f) =
  Compose . fmap (traverse g) . traverse f
```

- For `sequenceA` function:

```
t . sequenceA = sequenceA . fmap t
sequenceA . fmap Identity = Identity
sequenceA . fmap Compose =
  Compose . fmap sequenceA . sequenceA
```
