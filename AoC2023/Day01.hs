module Day01 where

import Control.Monad
import Data.Char (isDigit)
import HB.Ch11 (squishWith)
import System.IO

-- Pure ------------------------------------------------------------------------

-- Get the first and last digits in a string
firstLastDigits :: String -> String
firstLastDigits line = [head onlyDigits, last onlyDigits]
  where
    onlyDigits = filter isDigit line

sol :: [String] -> Int
sol = sum . (map (read :: String -> Int)) . (map firstLastDigits)

testIn =
  [ "1abc2",
    "pqr3stu8vwx",
    "a1b2c3d4e5f",
    "treb7uchet"
  ]

testOut = 142

stringDigits =
  [ ("one", "1"),
    ("two", "2"),
    ("three", "3"),
    ("four", "4"),
    ("five", "5"),
    ("six", "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("nine", "9")
  ]

-- convertStrDigits :: String -> String
-- convertStrDigits s = if dig then show num else s
--   where
--     subList = filter (\(sd, _) -> sd == s) stringDigits
--     dig = length subList == 1
--     num = snd $ subList !! 0

replace :: Int -> Int -> [a] -> [a] -> [a]
replace start size rep orig = take start orig ++ rep ++ drop (start + size) orig

findList :: (Eq a) => [a] -> [a] -> Int
findList query orig = go 0 query orig
  where
    go i _ [] = i
    go i q o = if q == take (length q) o then i else go (i + 1) q (tail o)

findAll :: (Eq a) => [a] -> [a] -> [Int]
findAll _ [] = []
findAll query orig =
  if first < length orig
    then [first]
    else [] ++ findAll query (drop (first + 1) orig)
  where
    first = findList query orig

findAndReplace :: (Eq a) => [a] -> [a] -> [a] -> [a]
findAndReplace query new orig =
  if findStart < length orig
    then findAndReplace query new newList
    else orig
  where
    findStart = findList query orig
    newList = replace findStart (length query) new orig

-- This doesn't work because it replaces all ones, then all twos, etc. rather than replacing the first digit.
convertAllDigits :: String -> String
convertAllDigits str =
  foldl
    (\s (strRep, dig) -> findAndReplace strRep dig s)
    str
    (reverse stringDigits)

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy f (x : xs) =
  (sortBy f [y | y <- xs, f x y == LT])
    ++ [y | y <- xs, f x y == EQ]
    ++ [x]
    ++ (sortBy f [y | y <- xs, f x y == GT])

-- In the case of overlaps, this will consume the shared characters, meaning the next value doesn't get converted.
-- i.e. "twone" becomes "2ne" instead of "21".
-- None of the examples in test2In have a trailing overlap, which might be my problem.
convertAllDigits' :: String -> String
convertAllDigits' str =
  if (\(digIdx, _, _) -> digIdx < length str) firstDigit
    then convertAllDigits' firstReplaced
    else str
  where
    allDigits =
      foldr
        (\(strRep, digStr) idxs -> idxs ++ [(i, strRep, digStr) | i <- findAll strRep str])
        []
        stringDigits
    sortedDigits = sortBy (\(i1, _, _) (i2, _, _) -> compare i2 i1) allDigits
    firstDigit = if sortedDigits == [] then (length str, "", "") else sortedDigits !! 0
    firstReplaced = (\(idx, rep, new) -> replace idx (length rep) new str) firstDigit

getAllDigits :: String -> [(Int, String)]
getAllDigits str = alreadyDigits ++ spelledDigits
  where
    spelledDigits =
      foldr
        (\(strRep, digStr) idxs -> idxs ++ [(i, digStr) | i <- findAll strRep str])
        []
        stringDigits
    alreadyDigits =
      foldr
        (\(strRep, digStr) idxs -> idxs ++ [(i, digStr) | i <- findAll digStr str])
        []
        stringDigits

onlyDigitsInOrder :: String -> String
onlyDigitsInOrder =
  (foldr (\(_, c) l -> c ++ l) "")
    . (sortBy (\(i1, _) (i2, _) -> compare i2 i1))
    . getAllDigits

sol2 :: [String] -> Int
sol2 = sol . (map convertAllDigits)

sol2' :: [String] -> Int
sol2' = sol . (map convertAllDigits')

sol2'' :: [String] -> Int
sol2'' = sol . (map onlyDigitsInOrder)

test2In =
  [ "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen"
  ]

test2Out = 281

-- IO --------------------------------------------------------------------------

-- Utility for reading in multiple lines (https://stackoverflow.com/a/15798324)
getLines :: IO [String]
getLines = do
  x <- getLine
  if x == ""
    then return []
    else do
      xs <- getLines
      return (x : xs)

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 01!"
  putStrLn "Which puzzle would you like to solve? (1 or 2 (negative for debug)):"
  puzNum <- getLine
  ifHandle <- openFile "AoC2023/input/Day01.txt" ReadMode
  raw <- hGetContents ifHandle
  case (read puzNum) :: Int of
    1 -> print $ sol $ lines raw
    2 -> print $ sol2'' $ lines raw
    -- (-2) -> do
    --   let ll = lines raw
    --   let converted = map convertAllDigits ll
    --   let fl = map firstLastDigits converted
    --   print $ "Number of lines: " ++ (show $ length ll)
    --   print $ "Number of converted: " ++ (show $ length converted)
    --   print $ "Number of FL digit pairs: " ++ (show $ length fl)
    --   d1 <- openFile "AoC2023/debug/d1-p2-1.txt" WriteMode
    --   d2 <- openFile "AoC2023/debug/d1-p2-2.txt" WriteMode
    --   hPutStr d1 $ squishWith '\n' converted
    --   hPutStr d2 $ squishWith '\n' fl
    --   hClose d1
    --   hClose d2
    _ -> print "Invalid Puzzle or no debug mode."
