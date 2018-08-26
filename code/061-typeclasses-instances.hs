-- dummy code to explain typeclass instances
class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

newtype Age =
  Age Integer
  deriving (Show, Eq)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year =
  Year Integer
  deriving (Show, Eq)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA = toNumber a
        integerOfAPrime = toNumber a'
        summed = integerOfA + integerOfAPrime

-- without Num a here the code will fail to compile
-- "a" cannot be of any type as we are using "+" - has to be Num
add :: Num a => a -> a -> a
add x y = x + y

-- same case here if we don't specify Ord a
addWierd :: (Num a, Ord a) => a -> a -> a
addWierd x y =
  if x > 1
  then x + y
  else x


-- This will compile without Num constraint
-- because we are using a concrete type Int
-- concrete types don't need type constraint
-- because they either implement the typeclass or don't
-- and the compiler knows about that
add' :: Int -> Int -> Int
add' x y = x + y

-- Int has instances of Ord and Num both
addWierd' :: Int -> Int -> Int
addWierd' x y =
  if x > 1
  then x + y
  else x

-- Int also has instance of Eq
check' :: Int -> Int -> Bool
check' a a' = a == a'
-- Adding typeclass to concrete types does not add any info

-- Chapter Exercises
data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Show, Eq)
settleDown x = if x == Woot
               then Blah
               else x

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f(a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f _ a = f(a)
