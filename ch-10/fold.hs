import Data.Time

-- foldr
fold :: (a -> b -> b) -> b -> [a] -> b
fold _ z [] = z
fold f z (x:xs) = f x (fold f z xs)

-- foldl
myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl _ z [] = z
myfoldl f z (x:xs) = myfoldl f (f z x) xs

-- write function that takes first three letters of each string value in a list of strings
-- and concatenate that result into a final String
listf3 :: [String] -> String
listf3 = foldr (\a b -> (take 3 a) ++ b) ""

-- exercises

-- given
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9002
  ]

-- 1. write function that filters for DbDate values and returns list of UTCTime values inside them
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where f (DbDate time) acc = time:acc
        f _ acc             = acc

-- 2. same as above but for DbNumber
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where f (DbNumber num) acc = num:acc
        f _ acc              = acc

-- 3. gets the most recent date
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = (foldr max $ UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0)) . filterDbDate

-- 4. sums all DbNumber values
sumDb :: [DatabaseItem] -> Integer
sumDb = (foldr (+) 0) . filterDbNumber

-- 5. function that gets average of the DbNumber values
-- reusing above functions - but this iterates the list twice
avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral . sumDb $ xs) / (fromIntegral . length . filterDbNumber $ xs)

-- 5b. without iterating twice
avgDb' :: [DatabaseItem] -> Double
avgDb' xs = (fromIntegral sum) / (fromIntegral count)
  where (sum, count) = foldr f (0, 0) xs -- single foldr -- returns sum and count combined
        f (DbNumber num) (sum, count) = (sum + num, count + 1)
        f _ acc                       = acc

-- nth fibonacci using scan
fibsN :: Int -> Int
fibsN n = fibs !! (n - 1)
  where fibs = 1 : scanl (+) 1 fibs

-- factorial using scan
factorial :: Int -> Int
factorial n = fact !! n
  where fact = scanl (*) 1 [1..]

-- Chapter exercises
-- 1. given
stops  = "pbtdkg"
vowels = "aeiou"

-- 1a. function that takes inputs from `stops` and `vowels` and makes 3-tuples
-- of all possible stop-vowel-stop combinations
stopVowelStop :: [(Char, Char, Char)]
stopVowelStop = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

-- 1b. modify above to only return combinations that begin with `p`.
stopVowelStopP :: [(Char, Char, Char)]
stopVowelStopP = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

-- 1c. Nouns and verbs instead of stops and vowels
nouns = ["apple", "pear", "mango", "litchee"]
verbs = ["grow", "eat", "digest", "throw"]
nounVerbNoun :: [(String, String, String)]
nounVerbNoun = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

-- secret function rewrite
seekritFunc :: String -> Double
seekritFunc x = (fromIntegral (sum (map length wrds))) / (fromIntegral (length wrds))
  where wrds = words x

-- rewrite functions from previous chapter exercises now using fold instead of recursion

-- 1. `myOr` returns true if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2. `myAny` returns True if a -> Bool applied to any of the values in the list returns True
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

-- 3a. myElem using fold
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> a == x || b) False

-- 3b. myElem using any
myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (==x)

-- 4. reverse the list
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5. map in terms of fold
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a xs -> (f a):xs) []

-- 6. filter in terms of fold
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a xs -> if (f a) then a:xs else xs) []

-- 7. Squish flattens list of lists into a list
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8. squishMap maps a function over a list and concatenates the results
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> (f a) ++ b) []

-- 9. squish using squishMap
squish' :: [[a]] -> [a]
squish' = squishMap id

-- 10. takes comparison function and a list and returns the greatest element
-- based on the last value that the comparison returned GT for
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\a b -> if (f a b == GT) then a else b) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> if (f a b == GT) then b else a) (head xs) xs
