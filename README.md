# Haskell Book Code

Code and notes from reading [Haskell Book](http://haskellbook.com/) -
including all exercises.

#### Requirements:

For running some examples, you might need `Hspec`, `QuickCheck`, `checkers`, etc.
installed globally. Can be installed via `cabal`:

```sh
cabal install hspec QuickCheck checkers scotty
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
