module Day14 where

import Day08 (transpose)
import HB.Ch09 (isEmpty, splitAndDrop)
import HB.Ch11 (idxsOf)
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
