# State Monad

Defined as `newtype` like `Reader`:

```haskell
newtype State s a =
  State { runState :: s -> (a, s) }
```

* Previous state from each application is chained to the next one.
  Eliminates the need to pass state explicitly.

* `state` function is a constructor that takes a state-like function
  and embeds it in a transformer.

```haskell
state :: Monad m => (s -> (a, s)) -> StateT s m a
```
