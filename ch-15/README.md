# Algebra & Haskell

* Haskell community has been known for recognizing `abstract patterns` in code
  that have well-defined lawful representations in mathematics.
* `Algebra` means one or more operations over different `sets`.
* Sets in algebra correspond to `typeclasses` in haskell.
* By using variables in algebra, we don't care about what value goes in there,
  but we care about the rules of how to manipulate those variables without reference to the particular value.
  This is exactly what typeclasses and their instances do.
* Or put in different words, in haskell these algebras can be implemented with typeclasses,
  where typeclasses define the set of operations.
* Typeclass instance defines how each operation will perform for given type/set.

# Monoid

**Monoid is a binary Associative operation with an identity.**

* Simply put, monoid is a function that takes two arguments and follows two laws - `associativity` and `identity`.

* Associativity - arguments can be regrouped.
* Identity - there exists some identity value - return other argument unmodified.

* Monoid typeclass generalizes these laws across types.

```haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

* `mappend` - any two values (of type that implements Monoid typeclass) can be joined together.
* `mempty` - identity value.

* `<>` is the infix operator for `mappend`.

```haskell
(<>) :: Monoid m => m -> m -> m
```

### Laws of typeclasses

* Laws make up what algebras are. Laws provide us guarantees that let us build
  on solid foundations.

* Following are laws that Monoid instances must follow:

```haskell
-- left identity
mappend mempty x = x

-- right identity
mappend x mempty = x

-- associativity
mappend x (mappend y z) = mappend (mappend x y) z

mconcat = foldr mappend mempty
```

```haskell
-- identity example with infix operator
(Sum 1 <> Sum 2) <> (Sum 3) == (Sum 1) <> (Sum 2 <> Sum 3)
```

* `mappend` should be thought as more of a way of combining/reducing a set of values
  rather than two values - in general cases for other typeclasses
  than number addition and list concat.
* For `Bool`, two possible monoids - `conjunction` (`All`) and `disjunction` (`Any`).
* `conjunction` returns `True` if and only if all values appending are `True`.
* `disjunction` returns `True` if any of the appending values is `True`.

```haskell
All True <> All False -- False
Any True <> Any False -- True
```

* This boolean example shows mappending is less about combining
  and more about reducing.

* `Maybe` has more than 2 monoids - `First`, `Last` and combining underlying values.

* Monoids that follow `commutative` property are called `commutative monoids`.
  Commutative means order of arguments can be changed - result stays the same.

### Identity

* turns a function into identity
* Identities are defined in terms of an identity relationship with a function.
  eg. identity of `(+)` is `0`, for `(*)` it is `1`, for `(:)` it is `[]`.

# Semigroups

* Semigroups are Monoids without identity values.

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

* We are left with only one law - `associativity`:

```
(a <> b) <> c == a <> (b <> c)
```

* `NonEmpty` is example of datatype that can have instance of Semigroup
  but not Monoid.

```haskell
data NonEmpty a = a :| [a]
  deriving (Eq, Ord, Show)
```

* No identity value by design hence no Monoid.

* `:|` is infix data constructor that takes two type  arguments and guarantees
  `at least` one value of type a.
