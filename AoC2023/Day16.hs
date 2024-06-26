module Day16 where

import Day14 (tupleSum)
import System.IO
import Day03 (unique)

-- Puz 1 -----------------------------------------------------------------------

testInput =
  [ ".|...\\....",
    "|.-.\\.....",
    ".....|-...",
    "........|.",
    "..........",
    ".........\\",
    "..../.\\\\..",
    ".-.-/..|..",
    ".|....-|.\\",
    "..//.|...."
  ]

testOutput = 46

arrIdx :: [[a]] -> (Int, Int) -> a
arrIdx arr (i, j) = (arr !! i) !! j

oob :: [[a]] -> (Int,Int) -> Bool
oob arr (r,c) = (r >= length arr) || (r < 0) || (c >= length $ head arr) || (c < 0)

stepPath :: [[Char]] -> [((Int,Int),(Int,Int))] -> [[((Int,Int),(Int,Int))]]
stepPath pattern path = unique $ map (\n -> if oob pattern n then path else n:path) next
  where
    (here,dir) = head path
    thisChar = arrIdx pattern here
    next = case thisChar of
      '.' -> [(tupleSum here dir, dir)]
      '|' -> case dir of
        (1, 0) -> [(tupleSum here dir, dir)]
        (-1, 0) -> [(tupleSum here dir, dir)]
        (0, 1) -> [(tupleSum here (1, 0), (1, 0)), (tupleSum here (-1, 0), (-1, 0))]
        (0, -1) -> [(tupleSum here (1, 0), (1, 0)), (tupleSum here (-1, 0), (-1, 0))]
      '-' -> case dir of
        (1, 0) -> [(tupleSum here (0, 1), (0, 1)), (tupleSum here (0, -1), (0, -1))]
        (-1, 0) -> [(tupleSum here (0, 1), (0, 1)), (tupleSum here (0, -1), (0, -1))]
        (0, 1) -> [(tupleSum here dir, dir)]
        (0, -1) -> [(tupleSum here dir, dir)]
      '/' -> case dir of
        (1, 0) -> [(tupleSum here (0, -1), (0, -1))]
        (-1, 0) -> [(tupleSum here (0, 1), (0, 1))]
        (0, 1) -> [(tupleSum here (-1, 0), (-1, 0))]
        (0, -1) -> [(tupleSum here (1, 0), (1, 0))]
      '\\' -> case dir of
        (1, 0) -> [(tupleSum here (0, 1), (0, 1))]
        (-1, 0) -> [(tupleSum here (0, -1), (0, -1))]
        (0, 1) -> [(tupleSum here (1, 0), (1, 0))]
        (0, -1) -> [(tupleSum here (-1, 0), (-1, 0))]
      _ -> error "bad character"

getPaths:: [[Char]] -> ((Int,Int),(Int,Int)) -> [[((Int,Int),(Int,Int))]]
getPaths pattern (start,dir) = something
 where
  pth = concat $ stepPath pattern [(start,dir)]

concat $ stepPath pattern [(start,dir)]
concatMap (stepPath pattern) $ p

make pattern x
 | length x == 1 = stepPath pattern x
 | otherwise = unique $ concatMap (stepPath pattern) $ make pattern x

make pattern x =

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 16!"
