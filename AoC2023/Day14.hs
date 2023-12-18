module Day14 where

import Data.List (sort)
import Day08 (transpose)
import HB.Ch09 (isEmpty, splitAndDrop)
import HB.Ch11 (contains, idxsOf)
import HB.Ch12 (pairizeList)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInput =
  [ "O....#....",
    "O.OO#....#",
    ".....##...",
    "OO.#O....O",
    ".O.....O#.",
    "O.#..O.#.#",
    "..O..#O..O",
    ".......O..",
    "#....###..",
    "#OO..#...."
  ]

testOutput = 136

count :: (Eq a) => a -> [a] -> Int
count _ [] = 0
count item (x : xs) = (if x == item then 1 else 0) + count item xs

splitWithGrouping :: (Eq a) => a -> [a] -> [[a]]
splitWithGrouping _ [] = []
splitWithGrouping item l = front : mid : splitWithGrouping item rest
  where
    front = takeWhile (/= item) l
    mid = takeWhile (== item) $ drop (length front) l
    rest = drop (length front + length mid) l

colLoad :: [Char] -> Int
colLoad col =
  sum
    $ map
      (\(off, str) -> valueOf (length col) off (count 'O' str))
    $ filter (\(_, s) -> (not $ isEmpty s) && '#' /= head s) idxStarts
  where
    splits = splitWithGrouping '#' col
    idxEnds = foldl (\l s -> l ++ [((fst $ last l) + length s, s)]) [(0, "")] splits
    idxStarts = map (\((x, _), (_, y)) -> (x, y)) $ pairizeList idxEnds
    numRocksIn = map (count 'O')

valueOf :: Int -> Int -> Int -> Int
valueOf _ _ 0 = 0
valueOf len offset num = (len - offset) + valueOf len (offset + 1) (num - 1)

sol :: [String] -> Int
sol = sum . (map colLoad) . transpose

-- Puz 2 -----------------------------------------------------------------------

arrIdxsOf :: (Eq a) => a -> [[a]] -> [(Int, Int)]
arrIdxsOf item arr = concatMap (\(r, l) -> map (\i -> (r, i)) l) $ filter (not . isEmpty . snd) temp
  where
    temp = map (\(r, i) -> (r, idxsOf item i)) $ zip [0 ..] arr

toCoords :: [String] -> ([(Int, Int)], [(Int, Int)])
toCoords arr = (stationary, mobile)
  where
    stationary = arrIdxsOf '#' arr
    mobile = arrIdxsOf 'O' arr

data Direction = North | West | South | East deriving (Eq, Show)

isBetween :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isBetween (x1, y1) (x2, y2) (i, j) =
  (min x1 x2 <= i)
    && (i <= max x1 x2)
    && (min y1 y2 <= j)
    && (j <= max y1 y2)

tupleSum :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

negTuple :: (Int, Int) -> (Int, Int)
negTuple (x, y) = (-x, -y)

tilt :: (Int, Int) -> [(Int, Int)] -> Direction -> [(Int, Int)] -> [(Int, Int)]
tilt (nR, nC) static dir mobile = []
  where
    refDir = case dir of
      North -> (-1, 0)
      West -> (0, -1)
      South -> (1, 0)
      East -> (0, 1)
    negRefDir = negTuple refDir
    startWalks = case dir of -- Where to begin stepping from
      North -> [(0, n) | n <- enumFromTo 0 (nC - 1)]
      West -> [(n, 0) | n <- enumFromTo 0 (nR - 1)]
      South -> [(nR - 1, n) | n <- enumFromTo 0 (nC - 1)]
      East -> [(n, nC - 1) | n <- enumFromTo 0 (nR - 1)]
    stopWalks = case dir of
      North -> [(nR - 1, n) | n <- enumFromTo 0 (nC - 1)]
      West -> [(n, nC - 1) | n <- enumFromTo 0 (nR - 1)]
      South -> [(0, n) | n <- enumFromTo 0 (nC - 1)]
      East -> [(n, 0) | n <- enumFromTo 0 (nR - 1)]
    stepsFrom here = scanl (\h _ -> tupleSum h negRefDir) here [0 ..]
    next here =
      head $
        dropWhile
          (\h -> (not $ contains h static) && (not $ contains h stopWalks))
          (stepsFrom here)

spin :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
spin dims static = sort . (tt East . tt South . tt West . tt North)
  where
    tt = tilt dims static

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 14!"
  putStrLn "Which puzzle would you like to solve (1)?"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day14.txt" ReadMode
  raw <- hGetContents fHandle
  case read puzNum :: Int of
    1 -> (print . sol . lines) raw
    _ -> print "Invalid puzzle."
