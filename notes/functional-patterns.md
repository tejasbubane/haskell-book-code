# Functional Patterns

* Anonymous Functions:

```haskell
\a b -> a + b
```

* Shadowing variables:

Variables can be shadowed in local scope with the `let` syntax. It can also be seen in `ghci`.

* Pattern matching:
- Match data constructors / values
- Bind variables in successful matches
- Order matters

* Case Expressions
- Write all cases otherwise we would have written a partial function that might throw errors at runtime.

* Function composition:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

`f . g $ x` is same as `f (g x)`

* Pointfree notation is writing code without arguments:

```haskell
let addOne = (+1)
```

can be used as: `addOne 5`

Here the parameter to function `addOne` is implicit (which gets passed on as second parameter to `+`) due to currying.
