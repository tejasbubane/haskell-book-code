{-# LANGUAGE QuasiQuotes #-}

import Text.Trifecta
import Control.Applicative
import Data.Ratio ((%))
import Text.RawString.QQ

stop :: Parser Char
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse p =
  print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s =
  putStrLn ('\n' : s)

-- Parsing Exercise
-- 1. Make `one` and `oneTwo` parsers fail because they didn't exhaust input stream
oneF :: Parser ()
oneF = char '1' >> eof

oneTwoF :: Parser ()
oneTwoF = char '1' >> char '2' >> eof

-- 2. Use string to make Parser that parses "1", "12" and "123"
-- single parser should parse all three
oneTwoThree :: Parser String
oneTwoThree = string "123" <|> string "div" <|> string "mod"

-- Parse fractions from text input
badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

-- Exercise: Unit of Success
-- Parse `integer, eof` and return that parsed integer - fail in all other cases
parseInt :: Parser Integer
parseInt = do
  result <- integer
  eof
  return result

-- Alternative example
type NumberOrString =
  Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser NumberOrString
parseNos = do
  skipMany (oneOf "\n")
  v <- (Left <$> integer) <|> (Right <$> some letter)
  -- value returned is inside `Parser` constructor
  -- since `Parser` is ` `Functor` - jump over that structure
  -- to apply `Left`/`Right`
  skipMany (oneOf "\n")
  return v

-- Exercise: Try Try
-- make a parser that can parse either decimals or fractions
type DecimalFrac = Either Double Rational

parseDecimalFrac :: Parser DecimalFrac
parseDecimalFrac = (Left <$> (try double)) <|> (Right <$> (try parseFraction))

-- Main
main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneF:"
  testParse oneF
  pNL "oneTwoF:"
  testParse oneTwoF
  pNL "oneTwoThree:"
  print $ parseString oneTwoThree mempty "123"
  print $ parseString oneTwoThree mempty "div"
  print $ parseString oneTwoThree mempty "mod"

  -- parsing fractions
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty badFraction
  print $ parseString parseFraction mempty alsoBad

  -- exercise: unit of Success
  print $ parseString parseInt mempty "123"
  print $ parseString parseInt mempty "123abc"

  -- alternative example
  print $ parseString (some parseNos) mempty eitherOr

  -- Exercise: Try Try
  print $ parseString parseDecimalFrac mempty "12.23"
  print $ parseString parseDecimalFrac mempty "12/23"
