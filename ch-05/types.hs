data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

-- generic functions for currying and uncurrying
curried :: ((a,b) -> c) -> a -> b -> c
curried f a b = f (a, b)

uncurried :: (a -> b -> c) -> (a, b) -> c
uncurried f (a, b) = f a b

checkInHundred :: Int -> Bool
checkInHundred = (`elem`[1..100]) -- partially apply with second argument. This is bonkers!!
-- This^ is called "Sectioning" in haskell

checkTen :: (Num a, Eq a) => [a] -> Bool
checkTen = (10 `elem`) -- can also be written as: checkTen = elem 10

-- Exercises
functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

i :: a -> a
i x = x

c :: a -> b -> a
c x _ = x

c' :: a -> b -> b
c' _ y = y

r :: [a] -> [a]
r = tail

co :: (b -> c) -> (a -> b) -> a -> c
co f g x = f $ g x

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f x = f(x)

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"
sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"
sing = if (x > y) then fstString x else sndString y
       where x = "Singin"
             y = "Somewhere"

main :: IO ()
main = do
  print (1 + 2)
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
    where blah = negate 1

f :: Int -> String
f = undefined
g :: String -> Char
g = undefined
h :: Int -> Char
h = g . f

data A
data B
data C
q :: A -> B
q = undefined
w :: B -> C
w = undefined
e :: A -> C
e = w . q

data X
data Y
data Z
xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform (a, b) = (xz(a), yz(b))

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g = fst . g . f
