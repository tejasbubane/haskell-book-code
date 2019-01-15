# IO

#### Naive explaination:

* IO Monad is just an instance of the ST Monad where state is the real world.

* There are problems with this explaination. `RealWorld` is represented by
  nothing at all - gets removed during compile time. So it is not a state there for
  the programmer to be manipulated.

#### Why we need IO?

* To order operations and disable some sharing. GHC can do lot of reordering, delaying,
  sharing, inlining and other optimizations in pure code to increase performance.
  IO turns off these abilities.

* IO actions are enclosed in nested lambdas - nesting is only way to ensure sequence
  due to weak-head normal form of evaluation.

* Values of type `IO a` are a description of how you might get an `a`. It is `not a
  computation`, but a description of how to get value from `real world` - possibly
  performing effects into the real world along the way.

* Describing IO actions does not perform them.

* Way we evaluate these descriptions is by defining `main`.
  Everything inside `main` is within `IO` and everything is nested and sequenced
  as expected.

* IO does not disable sharing for everything - only the terminal value it reduces to.
  Values that don't depend on IO can still be shared, even with a larger IO action
  like `main`.

* Haskell language evaluates expressions and construts IO actions that get executed by
  `main` at some point later.

* Like all Monads, `IO` class has a `Monad` instance - it also has instances for
  `Functor` & `Applicative`.

* `fmap`: construct action which performs same effects but converts `a` to `b`.

```haskell
fmap :: (a -> b) -> IO a -> IO b
```

* `<*>`: construct action that performs effects of both function and value arguments,
  but also applies function to value:

```haskell
(<*>) :: IO (a -> b) -> IO a -> IO b
```

eg. `(++) <$> getLine <*> getLine`

* `join`: merges effects of nested IO action

```haskell
join :: IO (IO a) -> IO a
```

* `pure`/`return` which we know is wrapping of given value within monadic/applicative
  context - for IO can be considered as embedding value in recipe-creating environment.
  Where recipe refers to the description of getting value while performing side-effects
  as described above.

#### Monadic Associativity

* We never evaluate/execute IO actions directly. We only construct them and hand over
  to main to execute them - we don't do IO we just talk about them.

* It helps if we think of IO as recipes, we don't cook anything. We only create recipes
  and hand them over to our cook `main` who cooks them for us - just like we asked :)
