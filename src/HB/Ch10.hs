-- Folding Lists

module HB.Ch10 where

import Data.Char
import Data.Time

-- 10.1: Folds -----------------------------------------------------------------

-- In general, folds reduce spines of data.

-- 10.4: Fold Right ------------------------------------------------------------

myFoldR :: (a -> b -> b) -> b -> [a] -> b
myFoldR _ z [] = z
myFoldR f z (x : xs) = f x (myFoldR f z xs)

-- Testing spine vs. value evaluation
t1 = foldr (\_ _ -> 9001) 0 [1, 2, 3, 4, 5]

t2 = foldr (\_ _ -> 9001) undefined [1, 2, 3, 4, 5]

t3 = foldr (\x _ -> 9001) undefined [undefined, 2, 3, 4, 5]

t4 = foldr (\x _ -> 9001 + x) 1 [1, undefined, 3, 4, 5]

t5 = foldr (\x _ -> 9001 + x) 1 ([1, undefined, 3, 4, 5] ++ undefined)

-- 10.5: Fold Left -------------------------------------------------------------

myFoldL :: (b -> a -> b) -> b -> [a] -> b
myFoldL f z [] = z
myFoldL f z (x : xs) = myFoldL f (f z x) xs

-- foldr (^) 2 [3, 4]
-- = (^) 3 ((^) 4 2)
-- = 3 ^ (4 ^ 2)
-- = 3 ^ 16
-- = 43046721

-- foldl (^) 2 [3, 4]
-- = foldl (^) ((^) 2 3) [4]
-- = foldl (^) ((^) ((^) 2 3) 4) []
-- = ((^) ((^) 2 3) 4)
-- = ((^) (2 ^ 3) 4)
-- = (2 ^ 3) ^ 4
-- = 8 ^ 4
-- = 4096

-- 10.5 Exercises
-- 1: B,C
-- 2: foldl (flip (*)) 1 [1..3]
--    = foldl (*) ((*) 1 1) [2..3]
--    = foldl (*) ((*) ((*) 1 1) 2) [3]
--    = foldl (*) ((*) ((*) ((*) 1 1) 2) 3) []
--    = (*) ((*) ((*) 1 1) 2) 3
--    = (*) ((*) (1 * 1) 2) 3
--    = (*) ((1 * 1) * 2) 3
--    = ((1 * 1) * 2) * 3
--    = (1 * 2) * 3
--    = 2 * 3
--    = 6
-- 3: C
-- 4: A
-- 5:
fiveA = foldr (++) "" ["woot", "WOOT", "woot"]

fiveB = foldr max '\NUL' "fear is the little death"

fiveC = foldr (&&) True [False, True]

fiveD = foldr (||) False [False, True]

fiveE = foldr ((++) . show) "" [1 .. 5]

fiveF = foldr const 'a' ['1' .. '5']

fiveG = foldr const '0' "tacos"

fiveH = foldl (flip const) '0' "burritos"

fiveI = foldl (flip const) (ord 'z') [1 .. 5]

-- Unconditional Spine Recursion
-- foldl always traverses the entire spine because it recurses on itself "before" evaluating the given function.
-- foldr is not required to traverse the entire spine because the formulation is "f x (foldr f z xs)".
-- In foldr, the "xs" may not be necessary to complete the evaluation of "f".
-- In foldl though, there is no such "checkpoint" where we could compute a value from partial traversal.
-- It must hit the base case of [] and z.
-- Basically, don't use left fold because it's slow.

-- 10.6: Writing Fold Functions ------------------------------------------------
-- 10.6 Exercises
data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving
    (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1:
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map getDate . filter isDbDate
  where
    getDate (DbDate utc) = utc
    isDbDate it = case it of
      DbDate _ -> True
      _ -> False

-- 2:
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map getNumber . filter isDbNumber
  where
    getNumber (DbNumber num) = num
    isDbNumber it = case it of
      DbNumber _ -> True
      _ -> False

-- 3:
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4:
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

-- 5:
avgDb :: [DatabaseItem] -> Double
avgDb db = total / len
  where
    total = fromIntegral $ sumDb db :: Double
    len = fromIntegral $ length $ filterDbNumber db :: Double

-- 10.7: Folding and Evaluation ------------------------------------------------

-- 10.8: Summary ---------------------------------------------------------------
-- foldr "alternates" calls between foldr and f, whereas foldl does all self-calls and then all f-calls.
-- The alternation of foldr allows it to quit where it needs to.

-- 10.9: Scans -----------------------------------------------------------------
fibs = 1 : scanl (+) 1 fibs

-- Scans Exercises
-- 1:
fibs20 = take 20 fibs

-- 2:
fibsSub100 = takeWhile (< 100) fibs

-- 3:
facts = scanl (*) 1 [1 ..]

-- I didn't really pay attention to this chapter but they said scans aren't used very much anyway.

-- 10.10: Exercises ------------------------------------------------------------

-- Set 1:
-- 1:
stops = "pbtdkg"

vowels = "aeiou"

-- 1a:
doY :: a -> [a] -> [[a]]
doY x (y : ys) = [x, y, x] : doY x ys
doY _ [] = []

doX :: [a] -> [a] -> [[[a]]]
doX (x : xs) yy = doY x yy : doX xs yy
doX [] _ = []

lol :: [a] -> [a] -> [[a]]
lol a b = concat $ doX a b

lolBest :: [a] -> [a] -> [[a]]
lolBest xx yy = [[x, y, z] | x <- xx, y <- yy, z <- xx]

-- 1b:
lolHead :: [a] -> [a] -> [[a]]
lolHead (a : _) b = doY a b

-- 1c:
nouns = ["aerojet", "atlas", "cloud", "peach", "barrel"]

verbs = ["whirl", "free", "gaze", "think", "quiver"]

fragment :: [a] -> [a] -> [(a, a, a)]
fragment xx yy = [(x, y, z) | x <- xx, y <- yy, z <- xx]

-- 2: Average word length
-- 3:
avgWordLength :: (Fractional a) => String -> a
avgWordLength sentence =
  (fromIntegral $ sum $ map length $ words sentence)
    / (fromIntegral $ length $ words sentence)

test10_3 = avgWordLength "hello there fellow traveler a big ass spider is right behind you"

-- Set 2:
-- 1:
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2:
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

-- 3:
myElem1 :: (Eq a) => a -> [a] -> Bool
myElem1 x = myAny (== x)

myElem2 :: (Eq a) => a -> [a] -> Bool
myElem2 e = foldr (\x y -> (x == e) || y) False

-- 4:
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5:
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []

-- 6:
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x : y else y) []

-- 7:
squish :: [[a]] -> [a]
squish = foldr (\x y -> x ++ y) []

-- 8:
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x y -> f x ++ y) []

-- 9:
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10:
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = foldr (\x y -> if GT == f x y then x else y) x xs

-- 11:
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = foldr (\x y -> if LT == f x y then x else y) x xs
