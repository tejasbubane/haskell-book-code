# Composing Types

* Functors and applicatives are `closed under composition`.
  i.e. composing two functors gives another functor (same for applicatives).

* But composing two monads is `not necessarily` a monad.
  i.e. Monad is not closed under composition.

* Composing monads allows us to build up computations with multiple effects.
  eg. By stacking up Maybe and IO - we can perform IO while also handling the case
      for failure (handled by the Maybe Monad).

#### Functors compose

```haskell
newtype Compose f g a =
  Compose { getCompose :: f (g a) }
```

* `f` and `g` are type constructors that take one type parameter
  (they are of kind `* -> *`) and `a` is the concrete type.

* The kind of compose is:

```haskell
Compose :: (* -> *) -> (* -> *) -> * -> *
```

same as `(.)`:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

eg.

```haskell
Compose [Just (1 :: Int), Nothing] :: Compose [] Maybe Int
```

* Type constructors are functions - can take other type constructors as arguments too.
  This allows us to `compose types`!!
* Allows us to express arbitrarily nested types:

```haskell
v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]
```

* This means functors compose resulting in another functor i.e. `functors are closed under composition`.

#### Applicatives compose

* Refer [code](../code/25-composing-types.hs) to see applicatives can be easily composed.
  One of the advantages of using these over Monads. Monads don't compose this easily (require transformers).

## Monad Transformers

* Fundamental problem with composing two Monads lies in the impossibility of joining two unknown Monads.

* Monad transformer is a type constructor that takes a Monad as an argument and returns a Monad as a result.

* We need a way to get `one big bind` over types like `IO (Reader String [a])`
  where monad instances involved are `IO`, `Reader` and `[]`.

* We can get a Monad of two types as long as we know what `one` of the types is.

* Refer `IdentityT` transformer definition in [code](../code/25-composing-types.hs)
  Transformer is a wrapper around an existing monad and all we do while defining
  a wrapper tranformer is define how the inner monad's bind should work when there
  is additional structure around it.

* General pattern - we want to compose two polymorphic types `f` and `g` that each have Monad instance defined.

```haskell
f (g (f b))
```

* Monad-bind can't join those types, not without knowing the inner `g`.
  Because the issence of monad is `join`, but join works with same types `m (m a)`.
  Remember `bind` is nothing but `join` and `fmap`.

* We need to remove `g`. In case of `IdentityT`, we use `runIdentity` which returns `m b` - thus leaving us with
  `m (m b)` - which can be dealt with join.

* `f` in above example can be anything (polymorphic) as long as it has a Monad instance.

```haskell
m (T m b) -> m (m b) -> m b -> T m b
```

where `m` is polymorphic type and `T` is some concrete type the transformer is for.
