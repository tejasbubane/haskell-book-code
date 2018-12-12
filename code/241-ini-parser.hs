{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.Trifecta
import Control.Applicative
import Text.RawString.QQ
import Data.ByteString (ByteString)
import Data.Map as M
import Test.Hspec

-- parser for INI file format
-- header
headerEx :: ByteString
headerEx = "[blah]"

-- "[blah]" -> Section "blah"
newtype Header =
  Header String
  deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'
-- those operators mean brackets parsed & discarded
-- but parser (p) will remain as result
-- *> and <* are left and right applicative operators

parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  value <- some (noneOf "\n")
  skipEOL
  return (name, value)

-- skip end of line and whitespace beyond
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

commentEx :: ByteString
commentEx =
  "; last modified 1 April\
  \ 2001 by John Doe"
commentEx' :: ByteString
commentEx' = "; blah\n#woot\n  \n;hah"

-- Skip comments starting at the beginning of the line
-- Comments can begin with ; or #
skipComments :: Parser ()
skipComments =
  skipMany $ do _ <- char ';' <|> char '#'
                skipMany (noneOf "\n")
                skipEOL

sectionEx :: ByteString
sectionEx =
  "; ignore me\n[names]\nTejas=Bubane"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[names]
Tejas=Bubane
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
;;; more comments
[section]
host=haskell.org
alias=kewl

[ironman]
tony=stark
|]

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = Prelude.foldr rollup M.empty sections
  return (Config mapOfSections)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "Assigment Parsing" $
    it "can parse a simple assigment" $ do
      let m = parseByteString parseAssignment mempty assignmentEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("woot", "1")
  describe "Header Parsing" $
    it "can parse a simple header" $ do
      let m = parseByteString parseHeader mempty headerEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")
  describe "Comment parsing" $
    it "Can skip a comment before a header" $ do
      let p = skipComments >> parseHeader
          i = "; woot \n[blah]"
          m = parseByteString p mempty i
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")
  describe "Section Parsing" $
    it "Can parse a simple section" $ do
      let m = parseByteString parseSection mempty sectionEx
          r' = maybeSuccess m
          states = M.fromList [("Tejas", "Bubane")]
          expected' = Just (Section (Header "names") states)
      print m
      r' `shouldBe` expected'
  describe "INI parsing" $
    it "Can parse multiple sections" $ do
      let m = parseByteString parseIni mempty sectionEx''
          r' = maybeSuccess m
          sectionValues = M.fromList [("host", "haskell.org"), ("alias", "kewl")]
          ironmanValues = M.fromList [("tony", "stark")]
          expected' = Just (Config (M.fromList [ (Header "section", sectionValues)
                                               , (Header "ironman", ironmanValues) ]))
      print m
      r' `shouldBe` expected'
