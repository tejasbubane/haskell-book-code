import Text.Trifecta
import Control.Applicative
import Data.Maybe (fromMaybe)

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major    = Integer
type Minor    = Integer
type Patch    = Integer
type Release  = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

parseVer :: Parser (Major, Minor, Patch)
parseVer = liftA3 (,,) (integer <* char '.') (integer <* char '.') integer

-- parseRest :: Parser Release
-- parseRest = (NOSS (many letter) <|> NOSI integer) `sepBy` (char '.')

parseStr :: Parser NumberOrString
parseStr = NOSS <$> many letter

parseInt :: Parser NumberOrString
parseInt = NOSI <$> integer

-- parseMetadata = char '-' *> (((NOSS <$> (many letter)) <|> (NOSI <$> integer)) `sepBy` (char '.'))

parseExtra :: Parser [NumberOrString]
parseExtra = (parseStr <|> parseInt) `sepBy` (char '.')

parseRelease :: Parser Release
parseRelease = char '-' *> parseExtra

parseMetadata :: Parser Release
parseMetadata = char '+' *> parseExtra

parseSemVer :: Parser SemVer
parseSemVer = do
  (major, minor, patch) <- parseVer
  release <- optional parseRelease
  meta <- optional parseMetadata
  eof
  return $ SemVer major minor patch (fromMaybe [] release) (fromMaybe [] meta)

main = do
  print $ parseString parseSemVer mempty "2.1.1"
  -- This is not working properly - need to fix.
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  -- print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
