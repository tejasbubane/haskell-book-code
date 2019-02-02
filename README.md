# Haskell Book Code [Complete]

This is fairly `complete` code and notes from reading
[Haskell Book](http://haskellbook.com/) - including all exercises.

_(Apart from a couple exercises in monad transformers which I found too hard to digest)_

This is my first Haskell learning experience so there might be some errors, please point them out. I will be more than happy to be corrected. Thanks!

#### Requirements:

For running some examples, you might need `Hspec`, `QuickCheck`, `checkers`, etc.
installed globally. Can be installed via `cabal`:

```sh
cabal install hspec QuickCheck checkers scotty criterion containers vector
```

#### Usage:

Each chapter has a directory with all related files in it - one or more code files with exercise solutions and a `README` file with notes taken while reading the book.

All files can be run by loading each file in `ghci` and
tried out the functions or running `main` for tests. Directories inside any chapter
are `cabal/stack` projects and hence the usual stack commands:

```sh
stack build
stack test
stack ghci
```
