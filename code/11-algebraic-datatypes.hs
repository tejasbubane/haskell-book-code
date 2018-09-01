{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char

data Price = Price Integer deriving (Show, Eq)
data Size = Size Integer deriving (Show, Eq)

data Manufacturer = Mini | Mazda | Tata deriving (Show, Eq)

data Airline = PapuAir | Cataplus | United deriving (Show, Eq)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Show, Eq)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 30000)

dodge :: Vehicle
dodge = Plane PapuAir (Size 200)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
getManu _            = error "No manufacturer data for planes"

-- Deriving instances of custom typeclasses
-- need to add the language pragma `GeneralizedNewtypeDeriving` top of the file
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, Int) where
  tooMany (n, m) = n + m > 42

instance (Num a, TooMany a, Ord a) => TooMany (a, a) where
  tooMany (n, m) = n + m > 42

-- given
data OperatingSystem =
  GnuPlusLinux
  | OpenBSD
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSD
  , Mac
  , Windows
  ]
allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

-- function that generates all possible values of Programmer
allProgrammers :: [Programmer]
allProgrammers =
  [Programmer { os = os, lang = lang }
  | os <- allOperatingSystems, lang <- allLanguages]

-- binary search tree
data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node left n right)
  | n == x = Node left n right
  | x < n  = Node (insert x left) n right
  | x > n  = Node left n (insert x right)

mapTree :: (Ord a, Ord b) => (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left n right) =
  Node (mapTree f left) (f n) (mapTree f right)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree == mapExpected
  then putStrLn "mapTree passed!"
  else putStrLn "mapTree failed!"

-- convert binary trees to lists
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = x:(preorder left) ++ (preorder right)

testTree' = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [1, 3, 4]
     && preorder testTree' == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Preorder failed!"

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) =
  (inorder left) ++ [x] ++ (inorder right)

testInorder :: IO ()
testInorder =
  if inorder testTree == [3, 1, 4]
     && inorder testTree' == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Inorder failed!"

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) =
  (postorder left) ++ (postorder right) ++ [x]

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [3, 4, 1]
     && postorder testTree' == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Postorder failed!"

main :: IO ()
main = do
  mapOkay
  testPreorder
  testInorder
  testPostorder
  testFoldTree

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left x right) = (f x acc'')
  where acc' = foldTree f acc left
        acc'' = foldTree f acc' right

testFoldTree :: IO ()
testFoldTree =
  if foldTree (+) 0 testTree == 8
     && foldTree (+) 0 testTree' == 6
  then putStrLn "foldTree fine!"
  else putStrLn "foldTree failed!"

-- Vigenère cipher
convert :: (Char, Char) -> Char
convert (x, y) = chr ((pos x + pos y) `mod` 26 + 65)
  where pos k = ord k - 65

-- my implementation does not consider spaces
-- but fine for the moment
vigenere :: String -> String -> String
vigenere _ [] = []
vigenere keyword input =
  map convert (zip input $ cycle keyword)

-- Return True only if all values in first list appear in second - although not contiguous
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf (x:xs) ys =
  x `elem` ys && isSubsequenceOf xs ys

-- Split a sentence into words,
-- then tuple each word with the capitalized form of each
capitalizeWords :: String -> [(String, String)]
capitalizeWords =
  map (\w@(x:xs) -> (w, (toUpper x):xs)) . words

-- function that capitalizes single word
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = (toUpper x):xs

-- little incorrect - does not capitalize first word
capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . go . words
  where go [] = []
        go (x:[]) = [x]
        go (x:y:xs) =
          if last x == '.'
          then x:(capitalizeWord y):(go xs)
          else x:(go (y:xs))

-- skipped last 2 exercises
