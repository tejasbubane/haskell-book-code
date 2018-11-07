#### Functor of functions

```haskell
fmap (+2) (*3)
```

* Functions can be `fmapped` (`lifted`) over other functions.
  The context for functor here is a `partially-applied function`.

* In above example, `(*3)` gets applied first and then `(+2)`.
  Which is the same as: `(+2) . (*3)`

#### Applicative of functions

```haskell
let bbop = (+) <$> (+2) <*> (*3)
bbop 3
```

* Here `(+)` is applied to two arguments resulting from application of `(+2)`
  and `(*3)`. The two operations `(+2)` and `(*3)` happen in parallel. (?)

#### Monad of functions

```haskell
do
  a <- (+2)
  b <- (*3)
  return $ a + b
```

* `a` and `b` are slots waiting to be filled. As soon as one argument gets
  applied to `(+2)` and `(*3)`, they get filled and `(+)` can be executed.

* All these `Functor`, `Applicative` and `Monad` for function are waiting for
  one argument that will allow both functions to be evaluated.
  This is the idea of `Reader`.

* We use function composition because it lets us compose two functions without
  explicitly having to recognize the argument that will eventually arrive.

# Reader

```haskell
data (->) a b
```

* `->` has kind `* -> * -> *`. To define functor, we have to bring it down to `* -> *`
  by applying one argument hence `(->) r` i.e. `r ->`
* Thus we make `r` part of the structure (that we are not supposed to change).

And finally:

```haskell
newtype Reader r a =
  Reader { runReader :: r -> a }
```
where `r` is the type we are `reading in` and `a` is result of the function.

```haskell
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) =
    Reader $ \r -> f (ra r)
    -- OR simpler
    Reader $ (f . ra)
```
