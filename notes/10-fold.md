# Fold operator

* Foldr: (right associative)

```haskell
fold :: (a -> b -> b) -> b -> [a] -> b
fold _ z [] = z
fold f z (x:xs) = f x (fold f z xs)
```

* Foldl: (left associative)

```haskell
myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl _ z [] = z
myfoldl f z (x:xs) = myfoldl f (f z x) xs
```

* Folds as a general concept are called `catamorphisms`.
* Both `foldl` and `foldr` are provided by `Data.List`.
* Folding happens in 2 stages - traversal and folding (also called evaluation/reduction).
* Both traverse over the spine from left but associativity (evaluation) order differs.
  Hence names `foldl` and `foldr`.
* `foldr` replaces the `cons` operator with function and the last `[]` with given initial value.
* Lazy in evaluation.
* Both folds have function case condition `[]` or `x:xs`. In order to determine
  which one to take, it has to evaluate the first spine structure (not the value).
* So for fold to work, the first spine cannot be a bottom (eg. cannot be undefined).
* `scanl` and `scanr` are similar to `foldl` and `foldr` but return all intermediate stages of fold.

* Relationship between `foldl` and `foldr`:

```haskell
foldr f z xs = foldl (flip f) z (reverse xs)
```
