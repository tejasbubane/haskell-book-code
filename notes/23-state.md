# State Monad

Defined as `newtype` like `Reader`:

```haskell
newtype State s a =
  State { runState :: s -> (a, s) }
```

* Previous state from each application is chained to the next one.
