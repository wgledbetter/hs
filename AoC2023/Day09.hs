module Day09 where

import HB.Ch12 (pairizeList)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInput =
  [ "0 3 6 9 12 15",
    "1 3 6 10 15 21",
    "10 13 16 21 30 45"
  ]

testOutput = 114

sequencePattern :: [Int] -> [[Int]]
sequencePattern l
  | all (== 0) l = l : []
  | otherwise = l : sequencePattern (map (\(a, b) -> b - a) $ pairizeList l)

stepSequencePattern :: [[Int]] -> [[Int]]
stepSequencePattern (l : ls)
  | all (== 0) l = [l ++ [0]]
  | otherwise = (l ++ [last l + (last . head) restStepped]) : restStepped
  where
    restStepped = stepSequencePattern ls

nextInPattern :: [Int] -> Int
nextInPattern = last . head . stepSequencePattern . sequencePattern

parseInput :: [String] -> [[Int]]
parseInput = map (\l -> map read $ words l)

sol :: [String] -> Int
sol = sum . (map nextInPattern) . parseInput

-- Puz 2 -----------------------------------------------------------------------

test2Output = 2

stepBackSequencePattern :: [[Int]] -> [[Int]]
stepBackSequencePattern (l : ls)
  | all (== 0) l = [0 : l]
  | otherwise = ((head l - (head . head) restStepped) : l) : restStepped
  where
    restStepped = stepBackSequencePattern ls

previousInPattern :: [Int] -> Int
previousInPattern = head . head . stepBackSequencePattern . sequencePattern

sol2 :: [String] -> Int
sol2 = sum . (map previousInPattern) . parseInput

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 09!"
  putStrLn "Which puzzle would you like to solve (1 or 2)?"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day09.txt" ReadMode
  raw <- hGetContents fHandle
  case (read puzNum) :: Int of
    1 -> (print . sol . lines) raw
    2 -> (print . sol2 . lines) raw
    _ -> print "Invalid puzzle"
