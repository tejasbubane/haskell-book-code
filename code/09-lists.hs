import Data.Bool (bool)
import Data.Char

myHead :: [a] -> a
myHead (x:_) = x
myHead []    = error "Head not allowed on empty list!"

myTail :: [a] -> [a]
myTail (_:xs) = xs
myTail []     = error "Tail not allowed on empty list!"

myHead' :: [a] -> Maybe a
myHead' []    = Nothing
myHead' (x:_) = Just x

myTail' :: [a] -> Maybe [a]
myTail' []     = Nothing
myTail' (x:[]) = Nothing
myTail' (_:xs) = Just xs

-- Write your own `enumFromTo` definitions

eftBool :: Bool -> Bool -> [Bool]
eftBool b b'
  | b == b'   = [b]
  | b < b'    = [b, b']
  | otherwise = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd o o'
  | o < o'    = o:(eftOrd (succ o) o')
  | o == o'   = [o']
  | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a <= b     = a:(eftInt (succ a) b)
  | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar a b
  | a <= b     = a:(eftChar (succ a) b)
  | otherwise = []

-- Using `takeWhile` and `dropWhile`, write a function that takes a string and
-- returns list of strings, using spaces to separate the elements into words
myWords :: String -> [String]
myWords s = mySplit s ' '

-- Function that takes a string and returns a list of strings
-- using newline separators to break up the string
myLines :: String -> [String]
myLines s = mySplit s '\n'

-- meta function for above two exercises
-- Takes a char and splits string using `takeWhile` and `dropWhile`
mySplit :: String -> Char -> [String]
mySplit s c = go s c []
  where take        = (/= c) -- not matching the split character
        split       = (== c) -- matching the split character
        go "" c acc = reverse acc
        go s c acc  = go (dropWhile split . dropWhile take $ s) c ((takeWhile take s):acc)

-- List comprehension to square all odd numbers between 1 and 20
sqOdd :: [Int]
sqOdd = [x^2 | x <- [1..20], rem x 2 == 0]

-- Function to remove all lowercase letters from a string using `list comprehension`
remLower :: String -> String
remLower xs = [x | x <- xs, not $ x `elem` ['a'..'z']]

-- Function to return all vowels from the string
vowels :: String -> String
vowels xs = [x | x <- xs, x `elem` "aeiou"]

-- Given
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- Write expression that will make tuples of outputs of `mySqr` and `myCube`
myTup :: [(Int, Int)]
myTup = [(x, y) | x <- mySqr, y <- myCube]

-- alter above expression to use x & y values less than 50
myTup' :: [(Int, Int)]
myTup' = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- count elements in above two output lists
tupCount = length myTup
tupCount' = length myTup'

-- Following function is lazy on evaluating cells
-- only forces spine evaluation (see notes for spine and non-strict eval)
myLen :: [a] -> Int
myLen [] = 0
myLen (_:xs) = 1 + myLen xs
-- can be tested using list [1,undefined,2]

-- Filtering lists

mul3 :: [Int] -> [Int]
mul3 = filter (\x -> rem x 3 == 0)

len3 :: Int
len3 = length . mul3 $ [1..30]

remArticles :: String -> [String]
remArticles = filter (\x -> not (x `elem` ["the", "a", "an"])) . words

-- own version of standard function `zip`
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)

-- own version of standard function `zipWith`
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

-- rewrite zip in terms of zipwith
zip'' :: [a] -> [b] -> [(a,b)]
zip'' = zipWith' (,)

-- Chapter exercises
-- 2. Remove all uppercase letters out of a string
removeUpper :: String -> String
removeUpper = filter isUpper

-- 3. Capitalize first letter of string
capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = (toUpper x):xs

-- 4. above function recursive to all caps
capitalizeAll :: String -> String
capitalizeAll [] = []
capitalizeAll (x:xs) = (toUpper x):(capitalizeAll xs)

-- 5. Capitalize first letter and only return that as result
capitalizeFirst' :: String -> Char
capitalizeFirst' = toUpper . head

-- Caesar Cipher
convert :: (Int -> Int) -> String -> String
convert f = map cipher
  where pos x = ord x - 97
        cipher x = chr $ (f (pos x) `mod` 26) + 97

caesar :: Int -> String -> String
caesar c = convert (\p -> p + c)

unCaesar :: Int -> String -> String
unCaesar c = convert (\p -> p - c)

-- Own versions of standard functions
-- True if list has all elements as True
myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = x && myAnd xs

-- True if list has atleast one True
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- True if any element satisfies the given predicate
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = (f x) || (myAny f xs)

-- True if given element is present in list
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ []     = False
myElem e (x:xs) = (e == x) || (myElem e xs)

-- myElem using myAny
myElem' :: (Eq a) => a -> [a] -> Bool
myElem' e = myAny (==e)

-- Reverse list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- flatten list of lists into single list
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

-- map function over list and concat results
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ (squishMap f xs)

-- flattens list of lists into a list - reuse squishMap function
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- take comparison function, list & return greatest element based on comparison function
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "no maximum of empty list"
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:y:xs) =
  case f x y of
    GT -> myMaximumBy f (x:xs)
    otherwise -> myMaximumBy f (y:xs)

-- same as above for minimum
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "no minumum of empty list"
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:y:xs) =
  case f x y of
    GT -> myMinimumBy f (y:xs)
    otherwise -> myMinimumBy f (x:xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
