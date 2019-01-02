# Applicatives

* Monoid - defines `mappend` - joins two values together.
* Functor - defines `fmap` - lifts function over a structure.
* Applicatives are `monoidal functors` - both the function and value are inside
  the structure. After applying function to value using functor,
  both structures are joined using monoid.

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

* Every type that has `Applicative` instance by definition, must also have
  a `Functor` instance. Note that `Monoid` is not `required` by definition.
* Note that both structures (around function and around value) are the same type.
* Pure function wraps something into Functorial structure.

```haskell
fmap f x = pure f <*> x
```

* The monoidal part shows nicely in this example:

```haskell
("woo", (+1)) <*> ("hoo", 1)
-- ("woohoo", 2)
```

#### Applicatives over list:

```haskell
[(+1), (+2)] <*> [4,5,6]
-- [5,6,7,6,7,8]
```

* One function applies to all elements and returns a list. Lists for all
  functions are combined in the end using monoid.

* It is a common practice to use functor and applicative in tandom.

```haskell
(++) <$> (Just "tejas") <*> (Just "bubane")
-- Just "tejasbubane"
```

* General structure of applicative can be seen from these instances:

```haskell
instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) =
    (u <> v, f x)
```

```haskell
instance Applicative Maybe where
pure = Just
Nothing <*> _     = Nothing
_ <*> Nothing     = Nothing
Just f <*> Just a = Just (f a)
```

## Applicative Laws

* **Identity**

```haskell
pure id <*> v = v
```

* **Composition**

```haskell
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```

* **Homomorphism**

- Structure preserving map between two algebraic structures.
- The effect of applying a function that is embedded in some structure to a value
  that is embedded in some structure should be the same as applying a function to a
  value without affecting any outside structure.

```haskell
pure f <*> pure x = pure (f x)
```

- Applying function does not change the structure around in any way,
  combining those two structures is taken care of by monoid only.

* **Interchange**

```haskell
u <*> pure y = pure ($ y) <*> u
```
- `u` represents function embedded in some structure
- right side `($ y)` means `y` is there waiting to be picked up by some function and applied.
