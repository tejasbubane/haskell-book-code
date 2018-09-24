# Monad

* Current standard of Haskell uses monad for constructing and transforming IO actions,
  order implementations of Haskell did not.
* Monads are powerful, but they do not define Haskell.
* Monads as `applicative functors` - with some unique features.

```haskell
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

* Chain of dependency:

```
Functor -> Applicative -> Monad
```

### Core operations:

* Minimally complete definition of monad requires definition of `>>=`.
* `return` is same as pure - take value and return it inside monadic structure.
* `>>` is called `sequencing operator` - sequence two actions while discarding result of first.
* `>>=` is called `bind operator` - this is what makes monad special.
* `bind` `(>>=)` is like `fmap` (but this function itself returns
  monad structures) and then join to remove the nesting of structures
  created by `fmap`.

* Monad is not impure. Monadic functions are pure. `IO` is a datatype that allows for impure or effectful actions.
* We need to merge multiple IO actions into a single one and sequence them to perform - `monads` to that.

```haskell
import Control.Monad (join)
join $ putStrLn <$> getLine
```

* `putStrLn <$> getLine` returns `IO (IO ())` - which `join` converts to `IO ()`
  to be able to execute both and not just the outer one.

```haskell
join :: Monad m => m (m a) -> m a
```

* `do` is just syntactic sugar for `sequencing` (`>>`) and `binding` (`>>=`).

* `Maybe` monad is interesting:

```haskell
instance Monad Maybe where
  return = Just
  (Just x) >>= k  = k x
  Nothing  >>= _  = Nothing
```

## Monad Laws

* **Identity**

```haskell
m >>= return = m       -- right identity
return x >>= f = f x   -- left identity

-- both these laws mean `return` should be neutral
```

* **Associativity**

```haskell
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

#### Monadic composition:

* called `Kleisli composition`.

```
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
flip (.) ::         (a -> b)   -> (b -> c)   -> a -> c
```
