-- function to replace word `the` with `a` in given string
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe xs = unwords (map (go . notThe) $ words xs)
  where go Nothing = "a"
        go (Just s)  = s

-- recursive function that takes string, breaks into words
-- and counts number of instances of "the" followed by
-- a vowel-initial word
vowel :: Char -> Bool
vowel x = elem x "aeiou"

wordsNotThe :: String -> [Maybe String]
wordsNotThe = map notThe . words

zipAdj :: [a] -> [(a, a)]
zipAdj xs = zip xs $ tail xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs =
  foldl go 0 $ zipAdj (wordsNotThe xs)
  where go acc (Nothing, Just x)
          | vowel (head x) = acc + 1
          | otherwise = acc
        go acc _ = acc

-- Return number of letters that are vowels in a word
countVowels :: String -> Integer
countVowels = foldr count 0
  where count a b = if vowel a then b + 1 else b

-- validate word
newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | countVowels s <= halfSize = Just $ Word' s
  | otherwise = Nothing
  where halfSize = fromIntegral (length s) `div` 2

-- Natural to Integer numbers conversion and vice-versa
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = (natToInteger n) + 1

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just $ go n
  where go 0 = Zero
        go n = Succ $ go (n - 1)

-- Small library for Maybe
-- 1. Simple boolean checks for Maybe values
isJust :: Maybe a -> Bool
isJust (Just _)  = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2. Maybe catamorphism
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just a)  = f a

-- 3. feedback value
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe x (Just y) = y

-- 4. Converting List and Maybe
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- 5. We just drop the Nothing values from list
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x:(catMaybes xs)

-- 6. called "sequence"
-- If single Nothing, result is Nothing otherwise Just List
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:xs) = Nothing
flipMaybe ((Just x):xs) =
  case flipMaybe xs of
    Nothing -> Nothing
    (Just a) -> Just (x:a)

-- Small Library for Either
-- 1. collect lefts
lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where go (Left x) acc = x:acc
        go (Right _) acc = acc

-- 2. collect rights
rights' :: [Either a b] -> [b]
rights' = foldr go []
  where go (Left _) acc = acc
        go (Right x) acc = x:acc

-- 3. Pair of both results
-- can be done using above 2 functions but where's the fun in that?
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
  where go (Left x) (lefts, rights)  = (x:lefts, rights)
        go (Right y) (lefts, rights) = (lefts, y:rights)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a)  = Nothing
eitherMaybe' f (Right b) = Just $ f b

-- 5. General catamorphism of Either values
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 _ (Left a)  = f1 a
either' _ f2 (Right b) = f2 b

-- 6. Same as 4 but using 5
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e =
  either' (\_ -> Nothing) (\b -> Just (f b)) e

-- iterate function using direct recursion
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a:(myIterate f (f a))

-- unfoldr using direct recursion
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = go [] b
  where go acc b =
          case f b of
            Nothing -> acc
            Just (a, b) -> a:(go acc b)

-- myIterate using myUnfoldr
betterIterate :: (a -> a) -> a -> [a]
betterIterate f a =
  myUnfoldr (\x -> Just (x, f x)) a

-- binaryTree is back

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- unfold for Binary Tree
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
               Nothing -> Leaf
               Just (l, n, r) -> Node (unfold f l) n (unfold f r)

-- tree builder using unfold
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold positives 0
  where positives x =
          if n < x
          then Nothing
          else Just (x + 1, x, x + 1)
