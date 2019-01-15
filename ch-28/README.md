# Basic Libraries

### Benchmarking & Profiling

* Using `criterion`

* `whnf` and `nf` functions provided by criterion as name suggest evaluate given expression to
  weak head or normal forms respectively.
  eg. `whnf (Just undefined)` will stop eval at outer constructor `Just` and hence not give error.
      But `nf (Just undefined)` will eval entire expression and throw error.

* Profiling options are provided by ghc flags - CPU and heap memory usages both can be profiled.

* Memory profiling will involve constant applicative forms (CAFs) which include:
  - values
  - partially applied functions without named arguments
  - fully applied functions

* CAFs are shared throughout the module.

## Map

* Map, Set, Sequence, etc. are goodies defined in `containers` package.

```haskell
data Map k a
  = Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a)
  | Tip

type Size = Int
```

* Implemented as balanced binary search tree internally, `Tip` is the Leaf node.

* Association list is one cheap way to achieve what Maps do but way slower.

* For `Int` as key, better use `IntMap` or `HashMap` or `Vector`.

* Keys should have `Ord` constraint - this is how lookups are made faster (BST).

## Set

```haskell
data Set a
  = Bin [-# UNPACK #-} !Size !a !(Set a) !(Set a)
  | Tip

type Size = Int
```

* Effectively equivalent to Map without values (or Map type with unit values).

## Sequence

* Solves common problem with lists where you can only cons from the front -
  sequences allow appending from both ends.

```haskell
newtype Seq a = Seq (FingerTree (Elem a))

newtype Elem a = Elem { getElem :: a } -- ignore

data FingerTree a
  = Empty
  | Single a
  | Double {-# UNPACK #-} !Int !(Digit a)
           (FingerTree (Node a)) !(Digit a)
```

* `concat`s are also faster with sequence compared to lists.

## Vector

* Not in containers - separate library called `vector`.

* Vector is almost always what you want instead of an Array in Haskell.

```haskell
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(Array a)
    deriving (Typeable)
```

* Comes in many variants - boxed, unboxed, immutable, mutable, storable, etc. Above is plain version.

* `Unboxed` means it represents raw values without pointer indirection - can save a lot of memory
  but limited to primitive types. `newtype` can be unboxed if the value it contains is primitive (unboxable).

* Vectors are designed to make slicing (creating sub-array) much cheaper than lists. This is because
  on slice they return new wrapper around original underlying array with new index and offset.

* Vector library has loop fusion built-in, so rather than constructing 4 vectors because we are mapping 4 times
  it composes the maps and runs them once.

### Mutable Vectors

* Mutable vectors have two variants - `IO` and `ST` (state) monads.

* Both these variants are way faster than the pure persistent versions above.

* `ST` monad is based on state monad studied in previous chapters. It unfreezes data,
  mutates it then refreezes back and pretends everything is pure :)
  Nice thing is before returning the value, it is freezed. So the impurity never comes
  out of `ST` monad.
