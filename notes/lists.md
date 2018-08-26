# Lists

```haskell
data [] a = [] | a : [a]
```

* `[]` is type constructor as well as data constructor.
* In the term level code, `[]` is nullary data constructor (since it takes no arguments).
* List declaration as a whole is `sum` type (due to `|`), but the data declaration of
  non-empty list (using `:`) is product type since it takes two arguments
  (`a` **and** `[a]`). Similar to tuple `(a, b)`.
* Unlike tuple, `:` is recursive constructor - the `cons` operator.
* Lists are made up of zero or more `cons` cells. `cons` cell is a conceptual space that values reside in.
* The underlying structure that nests (connects) the `cons` cells is called `spine`.

```haskell
take 2 $ enumFrom 10
```

* `enumFrom` returns an infinite list, but when we apply `take 2` it just returns `[10, 11]`. This is because of haskell's lazy evaluation.

### List comprehensions

* Just like set comprehensions.

```haskell
[x^2 | x <- [1..5]]
```

* Can have multiple generator lists (generator means lists supplying data to the comprehension).

```haskell
[x^y | x <- [1..5], y <- [2,3]]
-- Returns: [1,1,4,8,9,27,16,64,25,125]
```
  In above case, function is applied to each possible pairing of values from two lists.
  Always rightmost generator is exhausted first, then second rightmost and so on.

* Can have condition (similar to guards) on generator lists:

```haskell
[x^3 | x <- [1..10], rem x 3 == 0] -- cubes of numbers divisible by 3
-- Returns: [27,216,729]
```

* Multiple conditions for multiple generators can be added in the end separate by commas.

* Generator lists don't need to be of same size/type - because tuple can be of different type eg `(Int, String)`:

```haskell
[(x, y) | x <- [1..5], y <- ['a','x']]
-- Returns: [(1,'a'),(1,'x'),(2,'a'),(2,'x'),(3,'a'),(3,'x'),(4,'a'),(4,'x'),(5,'a'),(5,'x')]
```

* Since strings are lists of chars, list comprehensions can be used directly on strings.


#### Lazy evaluation

* Lists has cells connected by a structure of `(:)` called spine. Functions can force evaluation of cells only or spine only or both.

* `length` forces spine evaluation, but not cells - coz cell values not required.

```haskell
length [1, undefined, 2]
-- this works even though lists contains `undefined` coz its never evaluated
```

* `sum` is a example that forces cell evaluation - coz in order to calculated
  sum, we need to evaluate the arguments.

```haskell
sum [1,undefined,2] -- this fails - since cannot evaluate `undefined`
```

* All haskell expressions are in weak head normal form (WHNF) by default.
  WHNF expression is evaluated upto atleast the first data constructor.

* For lists, only the first spine is evaluated - not even the first cell.

```
  :
 / \
_   _
```

* Our following `length` function operates on spine only:

```haskell
myLen :: [a] -> Int
myLen [] = 0
myLen _:xs = 1 + myLen xs
```

can be tested using list like `[1,undefined,2]`. Using underscore tells haskell
we are not using the variable and hence need not be evaluated.
Even if we write `x:xs` but not use x anywhere after `=` it still works.
Haskell is smart!

* This works as well:

```haskell
take 3 $ map (+1) [1,2,3,undefined] -- returns [2,3,4]
```
since `undefined` was never forced.

### zipping lists

```haskell
zip [1,2] [4,5,6,7] -- returns [(1,4),(2,5)]
```
zipping stops as soon as one of the list runs out of elements.
