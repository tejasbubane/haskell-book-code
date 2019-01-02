## Maybe

* To define types that have something or don't.

```haskell
data Maybe a = Just a | Nothing
```

## Either

* Define either this or that:

```haskell
data Either a b = Left a | Right b
```

* For any datatype, case expressions and pattern matching will work without an
  `Eq` instance but guards using (==) will not.

## Kinds of types

* Higher-kinded types - types of type constructors.
* concrete type represented by single `*`.
* types constructors which take one other type are denoted by `* -> *`. So on..

* `Maybe` requires one type to concretize while `Either` needs two.
```haskell
[] :: * -> *
[] Int :: *
Maybe :: * -> *
Maybe String :: *
Either :: * -> * -> *
Either Int Bool :: *
(->) :: * -> * -> *
```

* `*` is fot lifted types while `#` is for non-lifted types.
* `lifted type` means any data-type that you could define yourself - can be inhabited by bottoms like `undefined`.
* `unlifted type` means data-types of internal pointers and native machine types - cannot be inhabited by bottoms.
* `newtype` is only exception that is denoted by `*` but is unlifted.
* Hence `undefined` can never inhabit `newtype` itself - but the type inside newtype can be `undefined`.

### unfold

* `fold` is `catamorphism` - `unfold` is `anamorphism`.
* `fold` means reducing a structure to single value, `unfold` means generating structure from single value.

```haskell
iterate :: (a -> a) -> a -> [a]
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
```
