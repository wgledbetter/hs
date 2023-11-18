-- Lists

module HB.Ch09 where

import Data.Char
import HB.Ch07 (foldBool)

-- 9.4 -------------------------------------------------------------------------
-- I have no idea what "cons cells" or "spines" are.

-- 9.5: range-based construction -----------------------------------------------

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool False False = [False]
eftBool True True = [True]
eftBool True False = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
  | a < b = a : eftOrd (succ a) b
  | a == b = [a]
  | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a < b = a : eftInt (succ a) b
  | a == b = [a]
  | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar a b
  | a < b = a : eftChar (succ a) b
  | a == b = [a]
  | otherwise = []

eftGen :: (Enum a, Ord a) => a -> a -> [a]
eftGen a b
  | a < b = a : eftGen (succ a) b
  | a == b = [a]
  | otherwise = []

-- 9.6: Extracting Portions of Lists -------------------------------------------

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x : xs) = x : myTake (n - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 l = l
myDrop _ [] = []
myDrop n (x : xs) = myDrop (n - 1) xs

mySplit :: Int -> [a] -> ([a], [a])
mySplit 0 l = ([], l)
mySplit _ [] = ([], [])
mySplit n l = (myTake n l, myDrop n l)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile f (x : xs)
  | f x = x : myTakeWhile f xs
  | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile f (x : xs)
  | f x = myDropWhile f xs
  | otherwise = xs

-- 9.6 Exercises

-- 1:
myWords :: String -> [String]
myWords [] = []
myWords (c : ss)
  | c == ' ' = myWords ss
  | otherwise = myTakeWhile (/= ' ') (c : ss) : myWords (myDropWhile (/= ' ') ss)

-- 2:
splitAndDrop :: (Eq a) => a -> [a] -> [[a]]
splitAndDrop _ [] = []
splitAndDrop s (x : xs)
  | x == s = splitAndDrop s xs
  | otherwise = myTakeWhile (/= s) (x : xs) : splitAndDrop s (myDropWhile (/= s) xs)

myLines = splitAndDrop '\n'

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

ansP2 = myLines (firstSen ++ secondSen ++ thirdSen ++ fourthSen)

-- 3
-- Already done: splitAndDrop

-- 9.7: List Comprehensions ----------------------------------------------------

basic = [x ^ 2 | x <- [1 .. 10]]

withPredicate = [x ^ 3 | x <- [1 .. 10], rem x 2 == 0]

-- The first generator is the "primary index" in a sense. In this example, y "moves faster" than x.
multiGen1 = [(x, y) | x <- [1 .. 3], y <- [2 .. 4]]

-- 9.7 Exercises
mySqr = [x ^ 2 | x <- [1 .. 10]]

-- 1: [4,16,36,64,100]
-- 2: [(1,64), (1,81), (1,100), (4,64), (4,81), (4,100), (9,64), (9,81), (9,100), etc.]
-- 3: [(1,64), (1,81), (1,100), (4,64), (4,81)]

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem a (x : xs)
  | a == x = True
  | otherwise = myElem a xs

-- More comprehension exercises:
mySqr5 = [x ^ 2 | x <- [1 .. 5]]

myCube5 = [y ^ 3 | y <- [1 .. 5]]

-- 1:
pb1 = [(a, b) | a <- mySqr5, b <- myCube5]

-- 2:
pb2 = [(a, b) | a <- mySqr5, b <- myCube5, a < 50, b < 50]

-- 3:
pb3 = length pb2

-- 9.8: Spines and Non-Strict Evaluation ---------------------------------------
-- "The spine is the recursive series of cons constructors ':'."
-- "Spines are evaluated independently of values."
-- Weak Head Normal Form (WHNF):
-- - Expression has been evaluated up to a data constructor. Values may remain un-evaluated.
-- - Use `:sprint` in ghci to view current evaluation state.
-- In the case of lists, "Normal Form" refers to full evaluation of spine (constructors?) _and_ values.

-- 9.8 Exercises
-- 1: Will generate one value (1^2), and then hit bottom.
-- 2: Safe. The "undefined" value is never needed.
-- 3: Bottom. Sum requires evaluation of all values.
-- 4: Safe, I think. The "undefined" item is taking the place of a value and therefore doesn't need to be evaluated.
--    Yep.
-- 5: Bottom. In this context, "undefined" represents a list, not an item. It is therefore not possible to traverse the spine to completion.
-- 6: Safe. Only the first even value is requested.
--    This problem felt more complex because the list goes through `filter` and `take`.
-- 7: Bottom. No even values are available before reaching `undefined`.
-- 8: Safe
-- 9: Safe
-- 10: Bottom

-- 1: NF
-- 2: WHNF
-- 3: Neither
-- 4: WHNF?
-- 5: NF?
-- 6: Neither?
-- 7: WHNF

-- 9.9: Transforming Lists -----------------------------------------------------
-- `map` and `fmap`
-- A list is a functor?
-- "Lazy in the spine, strict in the leaves."

goodList = [1 .. 5]

badList = [10, 25, undefined]

gb = goodList ++ badList

safe = take 7 gb

-- 9.9 Exercises
-- 1: Bottom.
-- 2: Safe
-- 3: Bottom.
-- 4: markVowels
-- 5a: [1,4,9,16,25,36,49,64,81,100]
-- 5b: [1,10,20]
-- 5c: [15,15,15]
-- 6:
pb6 = map (\x -> HB.Ch07.foldBool x (-x) (x == 3))

-- 9.10: Filters ---------------------------------------------------------------

-- 1:
filterMultipleOf :: (Integral a) => a -> [a] -> [a]
filterMultipleOf i = filter (\x -> rem x i == 0)

-- 2:
countMultipleOf :: (Integral a) => a -> [a] -> Int
countMultipleOf i = length . filterMultipleOf i

-- 3:
dropArticles :: String -> [String]
dropArticles = (filter (\x -> not $ elem x articles)) . (splitAndDrop ' ')
  where
    articles = ["a", "an", "the"]

-- 9.11: Zipping Lists ---------------------------------------------------------

-- 1:
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

-- 2:
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

-- 3:
myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 = myZipWith (\x y -> (x, y))

-- 9.12: Final Exercises -------------------------------------------------------

-- Set 1:
-- 2: isUpper
filterUpper = filter isUpper

-- 3:
properNoun (c : cs) = toUpper c : cs

-- 4:
capAllRec [] = []
capAllRec (c : cs) = toUpper c : capAllRec cs

-- 5:
firstCapped = toUpper . head

-- Set 2: Ciphers (ooooh!)
wrap :: (Integral a) => a -> a -> a -> a
wrap lo hi val = lo + mod (val - lo) diff
  where
    diff = hi - lo + 1

charShift :: Int -> Char -> Char
-- charShift i = chr . (+ i) . ord
charShift i c
  | elem c ['a' .. 'z'] = chr $ wrap (ord 'a') (ord 'z') $ i + ord c
  | elem c ['A' .. 'Z'] = chr $ wrap (ord 'A') (ord 'Z') $ i + ord c
  | otherwise = chr $ (+ i) $ ord c

caesarEncode :: Int -> String -> String
caesarEncode shift = map $ charShift shift

caesarDecode :: Int -> String -> String
caesarDecode shift = map $ charShift (-shift)

-- Set 3:
-- 1:
myOr :: [Bool] -> Bool
myOr [] = False
myOr (b : bs)
  | b = True
  | otherwise = myOr bs

-- 2:
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . (map f)

-- 3:
myElem2 :: (Eq a) => a -> [a] -> Bool
myElem2 e = myAny (== e)

-- 4:
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- 5:
squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

-- 6:
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

-- 7:
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8:
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f l =
  if isEmpty gts
    then head l
    else myMaximumBy f gts
  where
    gts = filter ((== LT) . f (head l)) $ tail l

-- 9:
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMinimumBy f l =
--   if isEmpty lts
--     then head l
--     else myMinimumBy f lts
--   where
--     lts = filter ((== GT) . f (head l)) $ tail l
myMinimumBy = myMaximumBy . flip

-- 10:
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMaximumBy $ flip compare
