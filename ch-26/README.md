# Monad Transformers

Refer [code](../code/26-monad-transformers.hs) for Functor, Applicative
and Monad instances of each of these:

#### MaybeT

```haskell
newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }
```

#### EitherT

```haskell
newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }
```

#### ReaderT

This is one of the most commonly used transformers in conventional Haskell apps.
`Reader` with additional monadic structure wrapped around the result.

```haskell
newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }
```

#### StateT

`State` with additional monadic structure wrapped around the result.

```haskell
newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }
```

All of these transformer types have been defined in `transformers` library.
Along with one interesting but rather less used type:

#### RWST

This type in `transformers` library combines Reader, Writer and State into one big type:

```haskell
newtype RWST r w s m a =
  RWST { runRWST :: r -> s -> m (a, s, w) }
```

* Correspondence between `StateT` and `Parser`:

```haskell
newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

type Parser = StateT String Maybe
```

String is input text, result is either successful parse or failure (Maybe).

### Base Monad

In a stack of Monad transformers, the `base monad` means what is
structurally `outermost`.

## MonadTrans

The typeclass that `lifts`

```haskell
class MonadTrans t where
  lift :: Monad m => m a -> t m a
```

`t` is a constructed monad transformer type that has `MonadTrans` instance defined.

* I guess this lifts a given monad into another monad transformer `t`.

* Monad transformers that the `Scotty` web framework relies on are in
  themselves newtypes for monad transformer stacks.

```haskell
newtype ScottyT e m a =
  ScottyT { runS :: State (ScottyState e m) a }
  deriving (Functor, Applicative, Monad)
```

This is some better way to reduce `lift`s - I did not understand this part :(

## MonadIO

* `MonadIO` is intended to keep lifting your IO action until it is lifted over all
  structure embedded in the outermost IO type.

* Alows not requiring to lift multiple times if you're trying to reach
  base (outermost) monad that happens to be IO - using `liftIO`.

```haskell
-- Defined in `Control.Monad.IO.Class`:
class (Monad m) => MonadIO m where
  -- Lift a computation from the 'IO' monad.
  liftIO :: IO a -> m a
```

* Any monad built by applying a sequence of monad transformers to the IO monad can
  be an instance of this MonadIO class.

* Example usage of `MaybeT`:

```haskell
-- Scotty provides param function but raises an exception if param not found
-- Assume a safe version param' which returns (MaybeT ActionM a)

type Reco = (Int, Int, Int)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word" -- word param is present (empty string if not provided)
    reco <- runMaybeT $ do
      a <- param' "1"
      b <- param' "2"
      c <- param' "3"
      return ((a, b, c) :: Reco)
    liftIO $ print reco
    html "<h1>Testing</h1>"
```

* The `one big bind` over `MaybeT` mean we could take the existence of
  `a`, `b` and `c` for granted within that context. But the `reco` value itself
  is `Maybe Reco` because any of those three params might be absent.

* It knows what monad we mean becaue the `do-block` is preceded by `runMaybeT`.

* _Sidenote:_ `Maybe` values are usually something contained in place local to where
  they are produced rather than allowing mysterious `Nothing` values floating around
  and short-circuiting our code.

* `Maybe` is something we use to supress error cases - to not allow end-users
  see the error. If we want end-users to know of the error, we use `ExceptT` aka
  `EitherT` (both are same). Code above remains the same for `Maybe` except that
  `param'` now returns `Except String ActionM a`
  (as opposed to `MaybeT ActionM a` before) from which the error in turn
  gets returned to user (as opposed to `Nothing` before).
