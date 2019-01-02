# Notes from Bartosxz Milewski lecture on monad:

Video links:

[1] Part 1: The builup - https://www.youtube.com/watch?v=PlFgKV0ZXoE
[2] Part 2: Defining Monads - https://www.youtube.com/watch?v=UtNB30Na65g
[3] Part 3: Curious case of IO Monad - https://www.youtube.com/watch?v=h6zbQ23U05g


#### Side effects:

For all cases - `Function are pure` - `takes value and returns value`.
We try representing some cases of side-effects using pure functions!

* **Example 1:** Function operating on a subset of values.
  eg. square root only works with positive numbers.

```haskell
a -> Maybe b
```

* **Example 2:** Function operating on value with some environment alongside:

```haskell
(a, e) -> b
-- with curring
a -> e -> b
-- same as
a -> (e -> b)
```

We are enhancing the return type with something extra.

* **Example 3:** Functions changing state:

```haskell
(a, s) -> (b, s) -- s is type not value so remains same (not changed to s' - but value will change)
-- same as
a -> (s -> (b, s))
```

* **Example 4:** Indeterministic results - eg. chess game

Since haskell is lazy, we can return `list` of all possible outcomes - even if they are infinite

```haskell
a -> [b]
```

* `a -> m b` is what we need to handle side-effects. `m` is a type constructor that encapsulates the side-effect.

* `m` in above examples is:
1. `Maybe a`
2. `Reader e`
3. `State s`
4. `[] a` (List)

* This `a -> m b` is called `Kleisli arrow`.

* Important to note: `Side effects are solved by Kliesli arrows - not monads.`

eg.

```haskell
sample :: Either a b -> Either a b
sample (Left x) = Left x
sample (Right x) = if x <= 0
                   then Left "Negative number"
                   else Right (x + 1)
```

* This pattern is very common in programming, especially the short-circuiting part
  where function returns for one case (`Left` in this case).
* Nice thing to note here is that the pattern sits between two functions:
  the caller and the function that handles right part
  (`if` condition in our example - but can be function).
* We want to be able to chain functions that take a data-type and return (usually)
  same type by doing some modifications based on constructor.
  In other words, we want to `compose` the side-effects.
* We need an abstraction that sits between functions. `composition` is one we know.
  But this one is different. We need composition that works on `a -> m b`.

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c) -- normal function composition
(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c) -- kliesli composition
```

* `>=>` is called `fish operator`. There is also inverse fish `<=<` with flipped arguments.

* Defining the fish operator for lists:

```haskell
(>=>) :: (a -> [b]) -> (b -> [c]) -> (a -> [c])
f >=> g = concat . fmap g . f
```

* `fish` is composition over `Kleisli arrows`.
* `m` (`[]` in above example) has to be a functor since we use `fmap` is used.
* If we want fish operator to be like composition, it also should have identity.
  But this identity is little different: `a -> m a` and its name is `return`.

```haskell
return :: a -> m a
```

* Return for different types:

- Maybe:

```haskell
return x = Just x
```

- Reader:

```haskell
Reader e a :: Reader (e -> a)
return x = Reader (\e -> x)
```

- State:

```haskell
State s a :: State ( s -> (b, s))
return a = State (\st -> (a, st))
```

- List:

```haskell
return x = [x]
```

#### Laws for Kleisli arrow:

Since `return` is identity of `Kleisli arrows`:

* **Identity**:

```haskell
return >=> f = f
f >= return = f
```

* **Associativity**:

```haskell
(f >=> g) >=> h  =  f >=> (g >=> h)
```

## Defining monad:

* Type constructor that has `return` and fish operator (`>=>`) defined -
  and follows the identity and associativity laws.
* Essense of `monad` is composition - its all about composition of Kleisli arrows.
* Fact that `Kleisli arrows` are so useful for doing side-effects is separate thing entirely.
* This is why monads are associated with side-effects, since if we want to
  compose side-effects (using Kleisli arrows), we have to use monads.
* The essence of monad is `composition`.

```haskell
class Functor m => Monad m where
  return :: a -> m a
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
  -- plus the three properties (left, right identity and associativity)
```
(This is not the same in prelude - but based on previous discussion).

* Instead of defining full fish, we define partial fish (`>>=`) - called bind.
  Where instead of the first function, we directly take the result of first function and pass it to second.

```haskell
class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
```
(This is how prelude defines `Monad`).
Here `m a` can be thought of as result of one function passed (composed) to second.

There's also:

```haskell
join :: m (m a) -> m a
```

* Defining monad for `Either`:

```haskell
instance Monad (Either a) where
  return = Right
  ma >>= f = case ma of
               Left e -> Left e -- Left is for error - short-circuit rest computation
               Right x -> k x
```

Can see pattern matching is nicely abstracted out from the function (refer `sample` function above).
So we don't have to repeat everywhere in all functions.

#### Syntactic sugar for Monads:

```haskell
safeSqrt :: Double -> Either String Double -- Assuming this is defined

safeSqrtRec :: Double -> Either String Double
safeSqrtRec ma = ma >>= if x == 1
                        then Left "div by zero"
                        else return (1/x)
```

Sugered version with `do`:

```
safeRec :: Double -> Either String Double -- Assuming this is also defined

safeSqrtRec ma = do
  x <- ma -- short circuits here and returns if ma has Left
  safeRec(x)
```

* We now have a way to code without considering error scenarios (or side-effects)
  since that is abstracted away by bind `(<-)`.

#### The State and IO Monads:

```haskell
main :: IO ()
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn "Hello " ++ name
```

`putStrLn` returns `IO ()` which is good in second usage but how does
the first line sequence?

Using another variant of bind `>>` which just ignores the result of computation
but interestingly does pass on the state to next function.

```haskell
(>>) :: ma -> mb -> mb
ma >> mb = ma >>= \_ -> mb
```

_Warning: Mind bending stuff ahead_

```haskell
s -> (a, s) -- State monad needs to be "run"
runState initState (State s a) = State s' a
```

* IO and pure functions are contradictory. But a nicely implemented in Haskell
  (using `State` monad internally).

* Note the above function is still pure - given a state it will always return
  another state with changes.

* But in `real world` there are infinite state possibilities. But haskell is lazy!
  So it keeps passing **ALL** possible states as `RealWorld` and keeps changing them
  as per the sequenced monadic operations. **!!Mind blown!!**

* Even though in `do` notation it looks like we get the value out of monad
  with `<-`, it is actually not so. We merely write functions to operate on data
  encapsulated in monads one after the other, pass it on to runtime from `main`
  which runs them and performs all the side-effecty operations.

* Our code remains pure while the haskell runtime does all the dirty job of
  actually performing the IO actions.
