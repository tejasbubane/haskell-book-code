# Functor

* Lift a function over a structure - means apply the function to value inside structure
  leaving structure unaltered.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

* The `f` in above definition is not to be confused as a function - it is a `struture` that has functor instance.
* Note the structure `f` is preserved from `fa -> fb` just the contents get transformed using function `(a -> b)`.
* List implements the typeclass `Functor` by `map` function, but `Functor` is a generalized concept
  and can be implemented by different datatypes.
  eg. `fmap (+2) (Just 3)` will return `Just 5`
* Infix notation for functors is `<$>`:

```haskell
<$> :: (a -> b) -> f a -> f b
```
This is little similar to function application `$`:

```haskell
$ :: (a -> b) -> a -> b
```
functor is lifting function over the structure and applying it on the values inside.

### Functor Laws:

* **Identity :**

```haskell
fmap id == id
```

* **Composition :**

```haskell
fmap (f . g) = (fmap f) . (fmap g)
```

* Both these laws enforce the essential rule that functors must be
  structure-preserving.
* Anything that breaks these laws is not a functor. Its just another function.


### Lifting multiple times

* `fmap` can be lifted multiple times using function composition:

```haskell
(fmap . fmap)
```

example:

```haskell
makeP = const p
names = [Just "alan", Nothing, Just "bob"]
fmap makeP names -- "ppp" [Char]
(fmap . fmap) makeP names -- [Just 'p', Nothing, Just 'p']
```

* Applying `fmap` multiple times using composition lifts the function
  over multiple boudaries - in above example, over `[]` and then `Maybe`.

* Exercise: Understanding how `fmap` composition typechecks:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> b
```

* Lets consider different names for both `fmaps` for understanding:

```haskell
fmap1 :: (x -> y) -> f1 x -> f1 y
fmap2 :: (m -> n) -> f2 m -> f2 n
```

* Think of curring while thinking composition of multi-argument functions.
  All functions take single argument and return either another value of function.

* so `fmap1` takes `(x -> y)` as argument returns `(f1 x -> f1 y)`
  output of `fmap1` is input to `fmap2` - function `(m -> n)` can be replaced by
  above output `(f1 x -> f1 y)`, which means `m == f1 x` and `n == f1 y`

```haskell
fmap2 :: (f1 x -> f1 y) -> f2 (f1 x) -> f2 (f1 y)
```

* Hence:

```haskell
(fmap . fmap) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
```

* We can compose `fmap` more times to jump over multiple layers of
  functorial structures: `fmap . fmap . fmap`

### Functors of Either and Maybe

* Very useful if you want to apply function over only `Just` (in `Either`) values
  or only the `Right` values (in `Maybe`) without caring about `Nothing` or `Left`.

### IO functors

* `fmap` can be used to lift over `IO` types as well.

```haskell
getInt :: IO Int
getInt = fmap read getLine
```

# Natural Transformations

* Transform only the structure leaving the contents alone.

```haskell
nat :: (f -> g) -> f a -> g a
```

This will not work since functions cannot take higher-kinded types as arguments (`f` and `g`).

```haskell
{-# LANGUAGE RankNTypes #-}
type Nat f g = forall a. f a -> g a
```

* It is useful to think of lifting as lifting over context rather than structure.
  Note that this lifting is metaphor.
