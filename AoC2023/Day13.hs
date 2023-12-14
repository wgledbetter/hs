module Day13 where

import Data.List (sortBy)
import Day08 (transpose)
import HB.Ch09 (splitAndDrop)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInput =
  [ "#.##..##.",
    "..#.##.#.",
    "##......#",
    "##......#",
    "..#.##.#.",
    "..##..##.",
    "#.#.##.#.",
    "",
    "#...##..#",
    "#....#..#",
    "..##..###",
    "#####.##.",
    "#####.##.",
    "..##..###",
    "#....#..#"
  ]

testOutput = 405

testInput1 =
  [ "#.##..##.",
    "..#.##.#.",
    "##......#",
    "##......#",
    "..#.##.#.",
    "..##..##.",
    "#.#.##.#."
  ]

testInput2 =
  [ "#...##..#",
    "#....#..#",
    "..##..###",
    "#####.##.",
    "#####.##.",
    "..##..###",
    "#....#..#"
  ]

isRefl :: (Eq a) => [a] -> Int -> Int -> Bool
isRefl l beg end
  | beg >= end = False
  | beg == end - 1 = (l !! beg) == (l !! end)
  | odd (end - beg) = ((l !! beg) == (l !! end)) && (isRefl l (beg + 1) (end - 1))

memoizeIntInt :: (Int -> Int -> a) -> (Int -> Int -> a)
memoizeIntInt fii = \x y -> ([[fii i j | j <- [0 ..]] | i <- [0 ..]] !! x !! y)

memListRefl :: (Eq a) => [a] -> Int -> Int -> Bool
memListRefl l = memoizeIntInt (isRefl l)

findRefls :: (Eq a) => [a] -> [(Int, Int)]
findRefls l =
  [ (a, b)
    | a <- enumFromTo 0 (length l - 2),
      b <- enumFromThenTo (a + 1) (a + 3) (length l - 1),
      memIsRefl a b
  ]
  where
    memIsRefl = memListRefl l

largestRefl :: [(Int, Int)] -> (Int, Int)
largestRefl = last . (sortBy (\(x1, y1) (x2, y2) -> compare (y1 - x1) (y2 - x2)))

reflsWithEnd :: (Eq a) => [a] -> [(Int, Int)]
reflsWithEnd l = filter (\(a, b) -> a == 0 || b == (length l - 1)) $ findRefls l

commonReflsWithEnd :: (Eq a) => [a] -> [a] -> [(Int, Int)]
commonReflsWithEnd l1 l2 = filter (\(s, e) -> memIsRefl2 s e) baseRefls
  where
    baseRefls = reflsWithEnd l1
    memIsRefl2 = memListRefl l2

commonReflsWithEnd' :: (Eq a) => [a] -> [(Int, Int)] -> [(Int, Int)]
commonReflsWithEnd' l2 = filter (\(s, e) -> memIsRefl2 s e)
  where
    memIsRefl2 = memListRefl l2

data ArrayRefls = ArrayRefls {rowRefls :: [(Int, Int)], colRefls :: [(Int, Int)]} deriving (Eq, Show)

arrayRefls :: (Eq a) => [[a]] -> ArrayRefls
arrayRefls m = ArrayRefls {rowRefls = rr, colRefls = cr}
  where
    c1 = reflsWithEnd (head m)
    cr = foldr commonReflsWithEnd' c1 $ tail m
    mT = transpose m
    r1 = reflsWithEnd (head mT)
    rr = foldr commonReflsWithEnd' r1 $ tail mT

nBeforeRefl :: (Int, Int) -> Int
nBeforeRefl (x, y)
  | y == 1 + x = x + 1
  | otherwise = nBeforeRefl (x + 1, y - 1)

arrayValue :: (Eq a) => [[a]] -> Int
arrayValue arr = rVal + cVal
  where
    rfls = arrayRefls arr
    rr = rowRefls rfls
    cr = colRefls rfls
    rVal = foldr (\rf val -> val + 100 * nBeforeRefl rf) 0 rr
    cVal = foldr (\rf val -> val + nBeforeRefl rf) 0 cr

parseInput :: [String] -> [[String]]
parseInput = splitAndDrop ""

sol :: [String] -> Int
sol = sum . (map arrayValue) . parseInput

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 13!"
  putStrLn "Which puzzle would you like to solve (1)?"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day13.txt" ReadMode
  raw <- hGetContents fHandle
  case read puzNum :: Int of
    1 -> (print . sol . lines) raw
    _ -> print "Invalid puzzle."
