import Data.List (intersperse)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ x = x
applyTimes n f x = f . applyTimes (n - 1) f $ x

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' 0 num = num
incTimes' n num = applyTimes n (+1) num

factorial :: (Num a, Eq a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci :: (Integral a) => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = (fibonacci (n - 2)) + (fibonacci (n - 1))

-- division in terms of recursive subtraction (until value becomes <= divisor)
dividedBy :: Int -> Int -> (Int, Int)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d    = (count, n)
          | otherwise = go (n - d) d (count + 1)
-- go is Haskell convention for making local function
-- which accepts more arguments than outer func
-- in this case accumulator (count) argument

-- Exercises
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- sum all numbers from 1 to n
sumAll :: (Eq a, Num a) => a -> a
sumAll 0 = 0
sumAll n = n + sumAll (n - 1)

-- multiply 2 integral numbers using recursive summation
mult :: (Eq a, Integral a) => a -> a -> a
mult x y = go 0 y
  where go acc 0 = acc
        go acc count = go (acc + x) (count - 1)


-- Fixing previous dividedBy to handle zero and negative numbers
data DividedResult = Result Int | DivideByZero deriving Show
divFixed :: Int -> Int -> DividedResult
divFixed num 0 = DivideByZero
divFixed num denom
  | (num > 0 && denom > 0) || (num < 0 && denom < 0) = Result $ go na da 0
  | otherwise = Result $ negate $ go na da 0
  where na = abs num
        da = abs denom
        go n d c
          | n < d = c
          | otherwise = go (n - d) d (c + 1)

-- McCarthy 91 function
mc91 :: Int -> Int
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91(mc91(n + 11))

-- numbers into words
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n = go n []
  where go 0 acc = acc
        go n acc = go (n `div` 10) ((n `mod` 10):acc)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
