module Lyah where

--------------------------------------------------------------------------------
----------------------------- Ch. 2: Starting Out ------------------------------
--------------------------------------------------------------------------------
5 /= 5 -- neq

max 0 5 -- Two args only

succ 9 + max 5 4 + 1 == (succ 9) + (max 5 4) + 1 -- Function precedence
succ 9 * 10 /= succ (9 * 10)

div 5 4 == 5 `div` 4 -- infix with `

-- Defining Functions ----------------------------------------------------------
doubleMe x = x + x

doubleMe 8

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

-- Lists -----------------------------------------------------------------------
[1, 2, 3, 4] ++ [9, 10, 11, 12]
'A' : " SMALL CAT"
5 : [1, 2, 3, 4, 5]

[9.4, 33.2, 96.2, 11.2, 23.25] !! 1

[3, 2, 1] > [2, 1, 0] -- True
[3, 2, 1] > [2, 10, 100] -- True
[3, 4, 2] > [3, 4] -- True
[3, 4, 2] > [2, 4] -- True
[3, 4, 2] == [3, 4, 2] -- True

-- head, tail, last, init, length, null, reverse, take, drop, minimum, maximum, sum, product, elem

[1 .. 20]
['a' .. 'z']
[2, 4 .. 20]
[3, 6 .. 20]
[20, 19 .. 1]

-- cycle, repeat, replicate

[x * 2 | x <- [1 .. 10]]
[x * 2 | x <- [1 .. 10], x * 2 >= 12]
[x | x <- [50 .. 100], x `mod` 7 == 3]

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

boomBangs [7 .. 13]

[x | x <- [10 .. 20], x /= 13, x /= 15, x /= 19]
[x * y | x <- [2, 5, 10], y <- [8, 10, 11]]
[x * y | x <- [2, 5, 10], y <- [8, 10, 11], x * y > 50]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

let xxs = [[1, 3, 5, 2, 3, 1, 2, 4, 5], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 4, 2, 1, 6, 3, 1, 3, 2, 3, 6]] in [[x | x <- xs, even x] | xs <- xxs]

-- Tuples ----------------------------------------------------------------------
fst (8, 11)
snd ("Wow", False)

zip [1, 2, 3, 4, 5] [5, 5, 5, 5, 5]
zip [5, 3, 2, 6, 2, 7, 2, 5, 4, 6, 6] ["im", "a", "turtle"]
zip [1 ..] ["apple", "orange", "cherry", "mango"]

triangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. 10], a <- [1 .. 10]]

rightTriangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2]

rightTriangles' = [(a, b, c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2, a + b + c == 24]

--------------------------------------------------------------------------------
------------------------- Ch. 3: Types and Typeclasses -------------------------
--------------------------------------------------------------------------------
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Int, Integer, Float, Double, Bool, Char

-- Type Variables --------------------------------------------------------------
head' :: [a] -> a
fst' :: (a, b) -> a
-- Typeclasses 101 -------------------------------------------------------------
-- Eq, Ord, Show, Read, Enum, Bounded, Num, Integral, Floating

--------------------------------------------------------------------------------
-------------------------- Ch. 4: Syntax in Functions --------------------------
--------------------------------------------------------------------------------

-- Pattern Matching ------------------------------------------------------------
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

let xs = [(1, 3), (4, 3), (2, 4), (5, 3), (5, 6), (3, 1)] in [a + b | (a, b) <- xs]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x : _) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x : []) = "The list has one element: " ++ show x
tell (x : y : []) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards ----------------------------------------------------------------------
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

-- Where -----------------------------------------------------------------------
bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

-- Let -------------------------------------------------------------------------
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

4 * (let a = 9 in a + 1) + 2

[let square x = x * x in (square 5, square 3, square 2)]

(let a = 100; b = 200; c = 300 in a * b * c, let foo = "Hey "; bar = "there!" in foo ++ bar)

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- Case ------------------------------------------------------------------------
head' :: [a] -> a
head' xs = case xs of
  [] -> error "No head for empty lists!"
  (x : _) -> x

describeList :: [a] -> String
describeList xs =
  "The list is " ++ case xs of
    [] -> "empty."
    [x] -> "a singleton list."
    xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where
    what [] = "empty."
    what [x] = "a singleton list."
    what xs = "a longer list."

--------------------------------------------------------------------------------
------------------------------- Ch. 5: Recursion -------------------------------
--------------------------------------------------------------------------------
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maximum' xs

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n -1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n -1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = a `elem'` xs

-- Quicksort -------------------------------------------------------------------
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted

--------------------------------------------------------------------------------
------------------------ Ch. 6: Higher Order Functions -------------------------
--------------------------------------------------------------------------------

-- Curried Functions -----------------------------------------------------------

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine :: (Num a) => a -> a -> a
multTwoWithNine = multThree 9

multWithEighteen :: (Num a) => a -> a
multWithEighteen = multTwoWithNine 2

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- Sectioning an infix function
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

divideTenBy :: (Floating a) => a -> a
divideTenBy = (10 /)

-- (sectioning an infix function)^-1
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

-- functions as arguments
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- Maps and filters ------------------------------------------------------------

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x : xs) =
  let smallerSorted = quicksort' (filter (<= x) xs)
      biggerSorted = quicksort' (filter (> x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter' p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

largestBeneathXDivisibleByY :: (Integral a) => a -> a -> a
largestBeneathXDivisibleByY x y = last (filter p [1 .. x])
  where
    p z = z `mod` y

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x : xs)
  | f x = x : takeWhile' f xs
  | otherwise = []

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | even n = n:collatz(div n 2)
  | odd n = n:collatz(3*n + 1)
