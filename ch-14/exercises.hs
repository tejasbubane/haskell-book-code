-- Chapter Exercises
module Exercises where

import Data.List
import Data.List (sort)
import Data.Char (toUpper)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof, frequency)

-- 1. Test `numbers into words` from recursion chapter
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
digits d = go d []
  where go 0 acc = acc
        go n acc = go (n `div` 10) ((n `mod` 10):acc)

wordNumber :: Int -> String
wordNumber xs = intercalate "-" $ map digitToWord (digits xs)

-- using hspec
specs :: IO ()
specs = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "returns one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

-- using QuickCheck

-- testing some simple arithmetic
half :: Int -> Double
half x = (fromIntegral x) / 2

-- this property should hold
halfIdentity :: Int -> Double
halfIdentity = (*2) . half

prop_halfIdentity :: Int -> Bool
prop_halfIdentity x = (fromIntegral x) == (halfIdentity x)

-- 2.
-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

prop_listOrdered :: [Int] -> Bool
prop_listOrdered = listOrdered . sort

-- 3. test associative and commutative properties of addition
prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z =
  x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y =
  x + y == y + x

-- 4. Same for multiplication
prop_multAssociative :: Int -> Int -> Int -> Bool
prop_multAssociative x y z =
  x * (y * z) == (x * y) * z

prop_multCommutative :: Int -> Int -> Bool
prop_multCommutative x y =
  x * y == y * x

-- 5. laws for relationship between quot and rem and div and mod
prop_quotRem :: Int -> Positive Int -> Bool
prop_quotRem x (Positive y) =
  (quot x y) * y + (rem x y) == x

prop_divMod :: Int -> Positive Int -> Bool
prop_divMod x (Positive y) =
  (div x y) * y + (mod x y) == x

-- 7. test reversing a list twice is same as identity
prop_reverseList :: (Eq a) => [a] -> Bool
prop_reverseList xs = xs == (reverse . reverse $ xs)

-- 8a. Property for definition of ($)
prop_apply :: (Eq b) => (Fun a b) -> a -> Bool
prop_apply (Fun _ f) a = (f $ a) == (f a)

-- 8b. property of function composition
prop_compose :: (Eq c) => (Fun b c) -> (Fun a b) -> a -> Bool
prop_compose (Fun _ f) (Fun _ g) a =
  (f . g) a == f (g a)

-- 9. properties of foldr
prop_concat1 :: (Eq a) => [a] -> [a] -> Bool
prop_concat1 xs ys = (foldr (:) ys xs) == (xs ++ ys)

prop_concat2 :: (Eq a) => [[a]] -> Bool
prop_concat2 xs = (foldr (++) [] xs) == (concat xs)

-- 10. length of list
-- fails if length xs < n
prop_take :: (Positive Int) -> [a] -> Bool
prop_take (Positive n) xs = length (take n xs) == n

-- 11. properties of read and show
prop_readShow :: (Read a, Show a, Eq a) => a -> Bool
prop_readShow x = (read (show x)) == x

-- Idempotence
twice :: (a -> a) -> a -> a
twice f = f . f
fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

-- Idempotence of capitalizeWord
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = (toUpper x):xs

prop_capitalizeWord :: String -> Bool
prop_capitalizeWord x = (capitalizeWord x == twice capitalizeWord x) &&
                        (twice capitalizeWord x == fourTimes capitalizeWord x)

-- Idempotence of sort
prop_sort :: (Ord a) => [a] -> Bool
prop_sort x = (sort x == twice sort x) &&
              (twice sort x == fourTimes sort x)

-- Write Arbitrary Instance
data Fool =
  Fulse | Frue deriving (Show, Eq)

instance Arbitrary Fool where
  arbitrary = oneof [return Fulse, return Frue]

-- Write Arbitrary instance with frequencies
data Dool =
  Dalse | Drue deriving (Show, Eq)

instance Arbitrary Dool where
  arbitrary = frequency $ [(2, return Dalse), (1, return Drue)]

-- Extra exercise
-- distributive property of reverse over (++)
prop_reverseConcat :: (Eq a) => [a] -> [a] -> Bool
prop_reverseConcat xs ys =
  reverse (xs ++ ys) == (reverse ys ++ reverse xs)

-- main to run them all
main :: IO ()
main = do
  putStrLn "number to words: hspec"
  specs

  putStrLn "Ex1: simple arithmetic: quickcheck"
  quickCheck prop_halfIdentity

  putStrLn "Ex2: ordered list"
  quickCheck prop_listOrdered

  putStrLn "Ex3: properties of addition"
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative

  putStrLn "Ex4: properties of multiplication"
  quickCheck prop_multAssociative
  quickCheck prop_multCommutative

  putStrLn "Ex5: properties of quot, rem, div and mod"
  quickCheck prop_quotRem
  quickCheck prop_divMod

  putStrLn "Ex7: property of reversed string"
  quickCheck (prop_reverseList :: [Int] -> Bool)
  quickCheck (prop_reverseList :: [Char] -> Bool)
  quickCheck (prop_reverseList :: [Bool] -> Bool)

  putStrLn "Ex8a: property of ($)"
  quickCheck (prop_apply :: (Fun Int String) -> Int -> Bool)
  quickCheck (prop_apply :: (Fun String Char) -> String -> Bool)
  quickCheck (prop_apply :: (Fun Bool Int) -> Bool -> Bool)
  putStrLn "Ex8b: property of (.)"
  quickCheck (prop_compose :: (Fun Int String) -> (Fun Double Int) -> Double -> Bool)
  quickCheck (prop_compose :: (Fun String Char) -> (Fun Bool String) -> Bool -> Bool)
  quickCheck (prop_compose :: (Fun String Int) -> (Fun Bool String) -> Bool -> Bool)

  putStrLn "Ex9a: property of foldr"
  quickCheck (prop_concat1 :: [Int] -> [Int] -> Bool)
  quickCheck (prop_concat1 :: [Bool] -> [Bool] -> Bool)
  quickCheck (prop_concat1 :: [Char] -> [Char] -> Bool)
  putStrLn "Ex9b: property of foldr"
  quickCheck (prop_concat2 :: [[Int]] -> Bool)
  quickCheck (prop_concat2 :: [[Bool]] -> Bool)
  quickCheck (prop_concat2 :: [[Char]] -> Bool)

  -- these fail when length xs < n
  -- putStrLn "Ex10: property of take"
  -- quickCheck (prop_take :: Positive Int -> [Int] -> Bool)
  -- quickCheck (prop_take :: Positive Int -> [String] -> Bool)
  -- quickCheck (prop_take :: Positive Int -> [Double] -> Bool)

  putStrLn "Ex11: property of read and show"
  quickCheck (prop_readShow :: Int -> Bool)
  quickCheck (prop_readShow :: Bool -> Bool)
  quickCheck (prop_readShow :: Char -> Bool)

  putStrLn "Idempotence of capitalizeWord"
  quickCheck prop_capitalizeWord
  putStrLn "Idempotence of sort"
  quickCheck (prop_sort :: [Int] -> Bool)
  quickCheck (prop_sort :: [Char] -> Bool)
  quickCheck (prop_sort :: [Double] -> Bool)

  putStrLn "Arbitrary instances"
  sample (arbitrary :: Gen Fool)
  sample (arbitrary :: Gen Dool)

  putStrLn "Extra exercise"
  quickCheck (prop_reverseConcat :: [Int] -> [Int] -> Bool)
  quickCheck (prop_reverseConcat :: [Bool] -> [Bool] -> Bool)
  quickCheck (prop_reverseConcat :: [String] -> [String] -> Bool)
