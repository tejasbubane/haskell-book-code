## Stack and Cabal

* `Cabal` is package manager.
* `Stack` is a build tool - uses `cabal` to some extent.

Some reference commands:

```sh
stack setup
stack build
stack ghchi
stack exec -- hello
```

## Modules

* By default, when you don't specify any exports in a module,
  every top-level binding is being exported and can be imported
  by another module.

#### Explicit Imports:

```haskell
import Data.Bool (bool)
```

#### Qualified Imports:

```haskell
import qualified Data.Bool
```

Have to access as `Data.Bool.bool` - fully qualified name.

Using alias:

```haskell
import qualified Data.Bool as B
```

Use as `B.bool`.

### Do block, <- and return

`do` block is used to sequence side-effects in a convenient syntax.

`<-` is special syntax in `do` blocks and is pronounced as `bind`. Result of binding (`<-`) over `IO String` is a `String`.

* In general `<-` binds `a` from `m a` where `m` is the monadic structure in above example `IO`.

* Each assignment using `<-` creates new variable because data is immutable.

```haskell
return :: Monad m => a -> m a
```

returns given value in monadic structure.
