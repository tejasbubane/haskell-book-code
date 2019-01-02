# Foldable

Class of data structures that can be folded to a `summary value`.

```haskell
class Foldable (t :: * -> *) where
  {-# MINIMAL foldMap | foldr #-}
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldMap :: (Data.Monoid m) => (a -> m) -> t a -> m
```

* `foldr` and `foldMap` can be defined in terms of each other hence `MINIMAL`
  is either (`|`).
* `Monoid` is used for combining the values and sometimes `mempty` in case
  the structure does not have any value inside eg. `Nothing`.

* `foldMap` has a function as it first arguments that maps values in
  structure `t` to monoid values.

  eg. `foldMap Sum [1,2,3] -- will return 6`

* Note from above example and type declarations above
  that the `structure is discarded`.
* `Foldable` is a way of generalizing `catamorphisms` (folding) to
  different datatypes.
* It may seem strange to fold on structures with single element inside, but
  think of it more like consuming the value rather than reducing it.


#### Derived operations of Foldables:

Implementations of these are derived from either `foldr` or `foldMap`.

```haskell
toList :: t a -> [a]

null :: t a -> Bool
-- true on Left and Nothing values, false otherwise

length :: t a -> Int
-- returns length of items in structure
-- not that length returns 0 for Nothing and Left values
-- even though Left has one value inside

elem :: Eq a => a -> t a -> Bool
-- does element occur in structure?

maximum :: Ord a => t a -> a
miminum :: Ord a => t a -> a
-- note: maximum Nothing raises exception

sum :: (Foldable t, Num a) => t a -> a
product :: (Foldable t, Num a) => t a -> a
```
