-- Anonymous Functions
addOne x = x + 1

addOne' = \x -> x + 1

isItTwo :: Integer -> Bool
isItTwo 2 = True

-- Pattern Matching
newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistered User"
printUser (RegisteredUser (Username name)
                          (AccountNumber num))
  = putStrLn $ name ++ " " ++ show num

-- another example of pattern matching
data WherePenguinsLive =
  Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False -- underscore means unconditional match

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng place) = place

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos (Peng Antarctica) = True
antarcticOrGalapagos (Peng Galapagos)  = True
antarcticOrGalapagos _                 = False

-- pattern matching on tuples
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

-- Case Expressions
funcZ x =
  case x + 1 == 1 of
    True -> "Awesome"
    False -> "wut"

pal xs =
  case xs == reverse xs of
    True -> "Yes"
    False -> "No"

functionC x y =
  case x > y of
    True -> x
    False -> y

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1

-- Higher order functions
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f x y = f y x


-- Guards
bloodNa :: Integer -> String
bloodNa x
  | x < 135   = "too low"
  | x > 145   = "too high"
  | otherwise = "just right" -- otherwise is just another name for True

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a^2 + b^2 == c^2 = "Right"
  | otherwise        = "Not right"

dogYrs :: Integer -> Integer
dogYrs x
  | x <= 0    = 0
  | x <= 1    = x * 15
  | x <= 2    = x * 12
  | x <= 4    = x * 8
  | otherwise = x * 6

-- function composition and point-free style
addPF :: Int -> Int -> Int
addPF = (+)

addFour :: Int -> Int
addFour = (+) 4

-- Exercises!! <3

-- 1.
tensDigit :: Integral a => a -> a
tensDigit x = mod (fst $ divMod x 10) 10

-- 2.
foldBool :: a -> a -> Bool -> a
foldBool x y pred =
  case pred of
    True -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y pred
  | pred = x
  | otherwise = y

foldBool'' :: a -> a -> Bool -> a
foldBool'' x y True = x
foldBool'' x y False = y

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 4.
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- 5.
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

-- 6.
roundTrip'' :: (Show a, Read b) => a -> b -- here GHC cannot infer type of b from a
roundTrip'' = read . show
-- so we need to call it like (roundTrip'' 4) :: Int (make output type explicit)
