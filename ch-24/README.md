# Parser Combinators

* Parser is a function that takes some textual input (string/bytestring/text etc)
  and returns some `structure` as output.

* Parsers analyze structure in conformance with rules specified in a grammar.

* Very basic parser:

```haskell
type Parser a = String -> Maybe (a, String)
```

* Takes a string, returns `Nothing` if parsing fails and
  `Just (parsed-value, remaining-string)` if parsing passes.
  This type is very similar to `State`.

* Another variant of parser type is `Hutton-Meijer` parser:

```haskell
type Token = Char
newtype Parser a = P ([Token] -> [(a, [Token])])

-- same thing
type Parser' a = String -> [(a, String)]
```

* Practical parsing libraries are complex but still behave a bit like `State`.
  Such that parsing things has an observable effect on one or more bits of state.

* Parsers can be combined flexibly allowing parsing lot of varied things.

eg. with `trifecta` library we can have something like:

```haskell
char '1' >> char '2' >> eof
```
which means parse character `1` followed by `2` followed by end of input.

#### Alternative typeclass

Defined in `Control.Applicative`:

```haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
```

In terms of parsing:
* `(<|>)` means `or` - either of left or right are success.
* `many` - zero or more
* `some` - one or more

#### QuasiQuotes

```haskell
{-# LANGUAGE QuasiQuotes #-}

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]
```

* This is a `macro` that lets us write arbitrary text inside of the block that
  begins with `[r|` and ends with `|]`.
* This conversion happens at compile time (hence `macro`).

* When writing parsers in Haskell, it is often easiest to work in terms of
  smaller parsers that deal with a sub-problem of the overall parsing problem,
  and then `combine` them into the final parser - `parser combinators`.

# Lexers and Parsers

* Lexers are fed a stream of characters which emits tokens on demand to the parser
  until it has no more to emit.
* Parser then structures the stream of tokens into a tree commonly called an
  `abstract syntax tree` or `AST`.

```haskell
lexer :: Stream Char -> Stream Token
parser :: Stream Token -> AST
```
Stream used for performance.

* Tokenization ignores whitespaces - but it isn't exclusively about whitespace,
  it's about ignoring noise so you can focus on structures you are parsing.

### Marshalling and unmarshalling

* Just parsing to AST isn't enough most times. We need the AST to be of
  particular form (while rejecting bad inputs) so our programs can work on them.
  This is called `unmarshalling`.

* Similarly, `marshalling` means converting data for serialization.

```haskell
Text -> Structure -> Meaning -- unmarshall
Meaning -> Structure -> Text -- marshalling
```
