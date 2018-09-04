-- Exercises
-- Refactor previous palindrom implementation to handle uppercased characters
-- and also punctuation marks
import Control.Monad (forever)
import Data.Char (toLower)
import System.IO

palindrome' :: String -> Bool
palindrome' str = cleanStr == (reverse cleanStr)
  where letters = map (\x -> toLower x) str
        cleanStr = filter (\x -> x `elem` ['a'..'z']) letters

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case palindrome' line1 of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"

main :: IO ()
main = palindrome

-- given
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise = Right $ Person name age

gimmePerson :: IO ()
gimmePerson = do
  -- do not buffer(defer) stdout(putStr)
  hSetBuffering stdout NoBuffering
  putStr "Please input your name: "
  name <- getLine
  putStr "Please input your age: "
  ageStr <- getLine
  let age = read ageStr :: Integer in
    case (mkPerson name age) of
      Right p ->
        putStrLn $ "Yay! Successfully got a person" ++ (show p)
      Left e  -> putStrLn $ show e
