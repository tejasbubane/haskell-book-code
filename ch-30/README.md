# When things go wrong

## Exceptions

* Exceptions are not explicitly part of interface (or types) - and hence program is
  difficult to reason about - use exceptions only when absolutely necessary.
  There are other better options to handle error cases like `Maybe` and `Either`.

* `Exception` class allows us to catch exceptions as well as define new ones.

```haskell
class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  displayException :: e -> String
  -- Defined in 'GHC.Exception'
```

* `SomeException` acts as sort of `parent` type for all other exception types.

```haskell
data SomeException =
  forall e . Exception e => SomeException e
```

* Note there is no type variable `e` in type constructor - only in data constructor.
  This changes the meaning of `for all e` to `there exists some e`. This is `existential quantification`.
  This means any type that implementes the `Exception` class can be that `e`.

* We need existential quantification because it enables us to throw different types of errors
  without unifying all of them under single sum type.

* We want hierarchy in which catching a `parent` means catching any of possible `children`.

## Typeable

* Know type of value at `runtime` and compare them - sorta like `dynamic typechecking`!

* In general this is not good, but makes sense with exception. Because we want to look at types of exceptions
  at `runtime` and catch particular ones.

* Typeable has this function `cast` that gets called from `fromException` in `Exception` class.

```haskell
cast :: (Typeable a, Typeable b) => a -> Maybe b
```

This is how it works:

exception occurs
-> traverse back in stack to check for `catch`
-> see if this exception type is handled
-> if no match we get `Nothing` from `cast` and `fromException` keeps going back looking in stack
-> if match `Just e` allows to catch the exception

## Exception Handling

* Exceptions may only be caught or handled in `IO`. Hence usually exception handling
  logic will remain in `main`.

```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

* calls `fromException` and `cast` - runs only if exception matches the one being handled.
  Way to handle exception and still return the desired IO action type.
  If no exception, `IO` (first arg) gets returned as is.

* Another way of handling exceptions is turning them into `Either` values:

```haskell
try :: Exception e => IO a -> IO (Either e a)
```

* Exception in Haskell are very similar to ones in other languages. If not handled,
  they'll bubble to the top and kill program.

## Throwing

```haskell
throw :: Exception e => e -> a
throwIO :: Exception e => e -> IO a
```

* Throwing can be thought of as a side-effect. Hence conventional way to throw
  an exception is using `throwIO` - which embeds the exception in an `IO`.

* Handling of exceptions must be done in IO even though they are thrown without IO.

* So we almost never use `throw` directly - it is used by internal library functions.

## Custom Exception Types

* Exception instances are derivable, we don't need to write the instance implementation. Don't know why deriving does not work though. Need to write empty exception.

```haskell
data MyException = MyException deriving (Eq, Show)

instance Exception MyException
```

* Related exceptions can be grouped together using sum types so that they can also be
  handled together.
