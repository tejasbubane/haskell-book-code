# Algebraic Datatypes

### data

```
data Bool = True | False
```

* constructors before `=` are type constructors - appearing in type-level code.
* constructors after `=` are data constructors - appearing in term/value level code.
* `|` denotes one-of (`or`) type.

* type and data constructors can take arguments:

```
data Node a b = Leaf | Left a b | Right a b
data [] a = [] | a : [a]
```

* Based on the number of arguments the constructors take they are called `nullary`, `unary`, `binary` etc.

* `True`, `False` are `nullary` data constructors, `Bool` is `nullary` type constructor. Also called data/type constants respectively.

* `[]` is example of `unary` data constructor. It requires a concrete type like `Int`, `String`, etc to create a value.

* The `Node` above is `binary` data constructor - accepts two type params `a` and `b`.
  This is an example of `product` type. (a `and` b)

* Types of types are called `kinds`. Can be inspected for type constructors using `:kind` or `:k` in `ghci`.

### type

This is a type synonym:

```haskell
type Age = Integer
```

`Age` henceforth will be exactly the same as `Integer`.

### newtype

Data-type with only single unary data constructor:

```haskell
newtype Goats = Goats Int
```

then use as:

```haskell
tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42
```

* One key difference between `type` and `newtype` is we can define new typeclass instances
  for `newtype` but not for `type`.
* `type` only creates new type constructor not data constructor.

#### Records

Sum types with convenient accessor functions:

```haskell
data Person = Person { name :: String, age :: Int }
```


* In algebra of types, sum types (`|`) are equivalent to addition,
  product types (`tuple` or `type constructor with two arguments`) to multiplication
  and function (`->`) to exponentiation. This analogy also helps us to figure out
  the number of possible values of each of these types.

* Operator with non-alphnumeric name is infix by default.

* Operator that starts with a colon must be infix type or data constructor.

### As-patterns

* As-patterns are way to pattern match and also refer to the whole structure:

```haskell
t@(a, b)
```
`t` refers to the entire tuple, `a` and `b` refer to elements inside.

```haskell
xs@(x:_)
```
`xs` refers whole list.
