# Non-Strictness

* Truly lazy language memoizes results of `all` functions it ever evaluates
  which will consume large amounts of memory for practical programs.

* Non-strictness means language should be able to evaluate expressions with bottoms
  in them as long as they (bottoms) are never evaluated.

* Implementations of Haskell are technically required to be `non-strict`
  rather than lazy, but for practical purposes lets call it `lazy`.

* Most expressions are only reduced/evaluated when necessary.
  Never needed - never evauluated. Unevaluated expressions are called `thunks`.
  The garbage collector (GC) sweeps the thunks away if never evaluated.

* So `GHC` is totally non-strict (as required by the specification of the language)
  and somewhat lazy (not truly lazy because of the garbage collection).

* `evaluation = thunk -> value`

* `Thunk` is placeholder in the underlying graph of the program.

* If thunk is evaluated, it can often be shared between expressions - this is laziness.

* The idea is that evaluation is driven by `demand`, not by construction.
  This also means we can never guarantee is something will ever be evaluated.

#### Outside in evaluation

* Strict languages evaluate `inside-out`, non-strict like Haskell do the opposite -
  they evaluate `outside-in`. Evaluation proceeds from outermost parts of expressions
  and works inward based on what values are forced.

Example:

```haskell
foldr' k z xs = go xs
  where
    go []     = z
    go (y:ys) = y `k` go ys

c = foldr' const 'z' ['a'..'e'] -- returns 'a'
```

`const` returns first argument without evaluating second,
hence our list tail never gets evaluated - so this `foldr'` will work even on
list with `undefined` in it - as long as it is not the first element.

`const` was in outermost position so it was evaluated first.

* Outside in evaluation is the reason why `length` of list never evaluates the
  elements of the list.

## Making Haskell Strict

* There are mechanisms to force evaluation

```haskell
seq :: a -> b -> b
```

* `seq` magically forces evaluation of first argument if and when the
  second argument has to be evaluated.
* It creates a graph of evaluations - forcing one exp will force another.
  eg.
  ```haskell
  a `seq` b `seq` c `seq d
  ```
  means `c` will be evaluated before `d`, but before that `b` but before that `a`
  must be evaluated. So `d -> c -> b -> a`.
* `seq` forces evaluation to weak head normal form (WHNF) - which means it stops at
  the first data constructor or lambda.

eg.

```haskell
let x = (,) undefined undefined
x `seq` 1 -- works
let y = \_ -> undefined
y `seq` 2 -- works as well
let z = undefined
z `seq` 3 -- does not work - undefined itself is bottom
```

* `x` and `y` in above example work because bottom is enclosed inside data constructor
  or function and haskell does not need to evaluate `inside` unless it is forced to
  (which seek does not) - this is `WHNF`.
* `case` also forces data constructor evaluation but not inner values
  (depends on nesting in the pattern).

## Call by *

* **Call by value:** args evaluated before entering function,
  exps evaluated before bindings - `strict` convention.

* **Call by name:** exps not necessarily evaluated - outside-in evaluation.

* **Call by need:** same as call by name, but evaluated only once.

* GHC oscillates between `by-need` and `by-name` based on optimizations.
  It can do this because of `pure` code.


* Sidenote: `:sprint` allows to see thunks in `ghci`. `_` is shown for unevaluated values.


## Sharing evaluations

* When a computation is named, results of evaluating that computation can be shared
  between all references to that name without re-evaluating.

* Imp to note sharing based on names not values. So `let x = 1` and `let y = 1`
  won't be shared. But in `let x = 1` and then `x + x` value is shared.

* Inlining expressions where they get used prevents sharing - thunks evaluated separately.

* Sharing doesn't work in presence of constraints like typeclasses - since they are
  functions in `Core` (GHC's intermediate language) which are waiting to be converted
  to concrete types - and unapplied functions are not shareable values.

  ie. in `Core`, `Num a => a` is really function `Num a -> a`.

  But it works if you give a concrete type.

* In short, polymorphic values maybe evaluated once but still not shared because, underneath,
  they continue to be functions awaiting application.

#### Preventing sharing

* We might want to prevent sharing if a large amount of data is there in order to compute a small value.
  We can discard the large data while preserving the small result.

* Trick to prevent sharing is turning values into `unit` functions:

```haskell
import Debug.Trace
let f x = (x ()) + (x ())
f (\_ -> trace "hi" 2)

-- hi
-- hi
-- 4
```

* Functions are not shared when there are named arguments but are when they are pointfree. :/

#### Forcing sharing

* Name the result using `let` binding. Names are what make values shareable.

#### Lazy patterns

Add tilde `~` before a pattern to make it lazy.

```haskell
strictPattern :: (a, b) -> String
strictPattern (a, b) = const "hello" a

lazyPattern :: (a, b) -> String
lazyPattern ~(a, b) = const "hello" a

strictPattern undefined -- error

lazyPattern undefined -- no error
-- "hello" returned
```

* `strictPattern undefined` above^ worked because we never needed `a` so pattern was never evaluated.

* Caveat is that lazy pattern cannot be used to discriminate cases of a sum.

#### Bang patterns

* Evaluate argument to function whether we use it or not

```haskell
f :: Int -> Int
f !x = 1
```

* Forcing something (with either `seq` or `!`) is expressed in `Core` as `case` expressions.

* We can specify bang pattern in data declarations.

```haskell
type Name = String
type Age = Int
data Person = Person !Name Age

age (Person _ a) = a

age (Person undefined 20) -- fails even if first argument is not used
```

* Idea here is sometimes its cheaper to just compute something than to construct thunk and then evaluate later.

## Strict/StrictData pragmas

* Avoid putting `seq` and `!` yourself. Makes everything strict in that module.

* Can use `~` to bring back laziness.
