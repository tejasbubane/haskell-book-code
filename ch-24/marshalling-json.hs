{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ
import Data.Text (Text)
import Data.Scientific (floatingOrInteger)

sectionJson :: ByteString
sectionJson = [r|
{ "section": {"host": "haskell.org"},
  "whatisit": {"red": "intoothandclaw"},
  "superhero": {"name": "Tony Stark"}
}
|]

data TestData =
  TestData {
    section :: Host
    , what :: Color
    , superhero :: SuperHero -- adding some of my own for practice
  } deriving (Eq, Show)

newtype Host = Host String deriving (Eq, Show)

type Annotation = String
data Color =
  Red Annotation
  | Blue Annotation
  | Yellow Annotation
  deriving (Eq, Show)

newtype SuperHero = SuperHero String deriving (Eq, Show)

instance FromJSON Host where
  parseJSON (Object v) =
    Host <$> v .: "host"
  parseJSON _ =
    fail "Expected an object for Host"

instance FromJSON Color where
  parseJSON (Object v) =
    (Red <$> v .: "red")
    <|> (Blue <$> v .: "blue")
    <|> (Yellow <$> v .: "yellow")
  parseJSON _ = fail "Expected an object of Color"

instance FromJSON SuperHero where
  parseJSON (Object v) =
    SuperHero <$> v .: "name"
  parseJSON _ = fail "Expected an object of SuperHero"

instance FromJSON TestData where
  parseJSON (Object v) =
    TestData <$> v .: "section"
             <*> v .: "whatisit"
             <*> v .: "superhero"
  parseJSON _ =
    fail "Expected an object for TestData"

-- Parsing number or string
data NumberOrString =
  Numba Integer
  | Stringy Text
  deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) =
    -- JSON does not have integers
    -- aeson uses Scientific to represent all numerical values
    -- we have to convert from scientific to integer
    case floatingOrInteger i of
      (Left _) -> fail "Must be integral number"
      (Right int) -> return $ Numba int
  parseJSON (String s) = return $ Stringy s
  parseJSON _ = fail "NumberOrstring must be number or string"

main = do
  print (decode [r|{"blue": "123"}|] :: Maybe Color)
  print (decode [r|{"red": "123"}|] :: Maybe Color)
  print (decode sectionJson :: Maybe TestData)

  print (decode "123" :: Maybe NumberOrString)
