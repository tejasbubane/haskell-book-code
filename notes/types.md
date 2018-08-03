# Notes Types & TypeClasses in Haskell

* `Term-level` code is where your values live and code that executes when your program is running.
  `Type-level` code is which is used during the static analysis & verification of your program.

* Haskell is built on `pure lambda calculus`.

* Functions, partially applied functions and values which are fully applied functions.
  Due to first-class currying, as in lambda calculus, all functions in haskell
  take single argument and return single result.

* By above definition, every function in Haskell that takes more than one parameter
  is a higher order function (since it always returns another function -
  the one waiting for more arguments).

* Arrow (->) is a type constructor for functions that takes two type arguments -
  input type and result type of the function.

    ```
    data (->) a b
    ```

`a` is input type, `b` is result type.

* Function type constructor does not have data constructors.

* Type variables in type constructors - the more constraints they have -
  more the functionality - the less constraints they have - lesser the functionality:

  ```
  id :: a -> a
  ```

  No type constraint on this identity function, can take any value - has no functionality.
  Since we have zero info about type, we cannot do anything with it.
  Concrete types have more flexibility in terms of computation -
  typeclasses make types variables more concrete by adding behaviour in them.
