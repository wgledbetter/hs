import Prelude hiding (and, filter, foldl, foldr, gcd, head, length, map, null, or, tail, (++))

-- Data ------------------------------------------------------------------------
data Minutes = Minutes Int

addMin :: Minutes -> Minutes -> Minutes
addMin (Minutes x) (Minutes y) = Minutes (x + y)

-- Codelab ---------------------------------------------------------------------
-- 01: Functions ---------------------------------------------------------------
add :: Int -> Int -> Int
add x y = x + y

subtract :: Int -> Int -> Int
subtract x y = x - y

double :: Int -> Int
double x = 2 * x

multiply :: Int -> Int -> Int
multiply x y = x * y

divide :: Int -> Int -> Double
divide x y = fromIntegral x / fromIntegral y

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n -1)

gcd' :: Int -> Int -> Int
gcd' a b
  | a < b = gcd' b a
  | remAB > 0 = gcd' b remAB
  | otherwise = b
  where
    remAB = rem a b

-- 02: Datatypes ---------------------------------------------------------------
hours :: Minutes -> Int
hours (Minutes mm) = div mm 60

timeDistance :: Minutes -> Minutes -> Minutes
timeDistance (Minutes mm1) (Minutes mm2)
  | mm1 < mm2 = timeDistance (Minutes mm2) (Minutes mm1)
  | otherwise = Minutes (mm1 - mm2)

type Point = (Int, Int)

pointDistance :: Point -> Point -> Double
pointDistance p1 p2 = sqrt (fromIntegral (x2 - x1)) ^ 2 + fromIntegral (y2 - y1) ^ 2
  where
    x1 = fst p1
    y1 = snd p1
    x2 = fst p2
    y2 = snd p2

-- 03: Lists -------------------------------------------------------------------
null :: [a] -> Bool
null [] = True
null _ = False

head :: [a] -> a
head [] = error "head: empty list"
head (x : _) = x

tail :: [a] -> [a]
tail [] = error "tail: empty list"
tail (_ : xs) = xs

length :: [a] -> Int
length [] = 0
length (x : xs) = 1 + length xs

and :: [Bool] -> Bool
and [] = error "and: empty list"
and [x] = x
and (x : xs)
  | not x = False
  | otherwise = and xs

or :: [Bool] -> Bool
or [] = error "or: empty list"
or [x] = x
or (x : xs)
  | x = True
  | otherwise = or xs

(++) :: [a] -> [a] -> [a]
[] ++ [] = []
l1 ++ [] = l1
[] ++ l2 = l2
l1 ++ (y : ys) = snoc l1 y ++ ys

-- 04: Abstractions ------------------------------------------------------------
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a : as) = f a : map f as

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs)
  | f x = x : filter f xs
  | otherwise = filter f xs

foldl :: (a -> x -> a) -> a -> [x] -> a
foldl _ a [] = a
foldl f a (x : xs) = foldl f (f a x) xs

foldr :: (x -> a -> a) -> a -> [x] -> a
foldr _ a [] = a
foldr f a (x : xs) = f x (foldr f a xs)

-- 05: Maybe -------------------------------------------------------------------
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

isNothing :: Maybe a -> Bool
isNothing (Just a) = True
isNothing a = False

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a _ = a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ f (Just a) = f a
maybe b _ _ = b
