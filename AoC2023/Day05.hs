module Day05 where

import Data.List (sort)
import Day03 (unique)
import HB.Ch09 (splitAndDrop)
import HB.Ch11 (contains)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInput =
  [ "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4"
  ]

testOutput = 35

data Map = Map {destStart :: Int, sourceStart :: Int, range :: Int} deriving (Eq, Show)

type MapGroup = [Map]

instance Ord Map where
  compare m1 m2 = compare (sourceStart m1) (sourceStart m2)

inputOf :: Map -> [Int]
inputOf (Map _ ss r) = take r [ss ..]

inputContains :: Map -> Int -> Bool
inputContains (Map _ ss r) i = i >= ss && i < ss + r

outputOf :: Map -> [Int]
outputOf (Map ds _ r) = take r [ds ..]

outputContains :: Map -> Int -> Bool
outputContains (Map ds _ r) i = i >= ds && i < ds + r

performMap :: Map -> Int -> Int
performMap m@(Map ds ss r) i
  | inputContains m i = ds + (i - ss)
  | otherwise = error "Out of range"

-- mapsOverlap :: [Map] -> Bool
-- mapsOverlap [] = False
-- mapsOverlap (m@(Map _ _ mSize) : ms) =
--   foldr hasOverlap False ms || mapsOverlap ms
--   where
--     im = inputOf m
--     low1 = head im
--     hi1 = last im
--     hasOverlap aMap@(Map _ _ mSize') previous = previous || (head im < last ia) || (head ia < last im)
--      where
--       ia = inputOf aMap
--       low2 = head ia
--       hi2 = last ia

performMapGroup :: MapGroup -> Int -> Int
performMapGroup ms i = if theMaps == [] then i else performMap (head theMaps) i
  where
    sortedMaps = sort ms
    theMaps = filter (\m -> inputContains m i) ms

applyAllMaps :: [MapGroup] -> Int -> Int
applyAllMaps mgs i = foldl (flip performMapGroup) i mgs

parseMapLine :: String -> Map
parseMapLine s = Map (nums !! 0) (nums !! 1) (nums !! 2)
  where
    nums = map read $ words s :: [Int]

parseMapGroup :: [String] -> MapGroup
parseMapGroup = map parseMapLine

parseInput :: [String] -> ([Int], [MapGroup])
parseInput ss = (seeds, mapGroups)
  where
    groups = splitAndDrop "" ss
    seedGroupStr = head groups
    mapGroupStrs = tail groups
    seeds = map read $ words $ ((splitAndDrop ':' $ head seedGroupStr) !! 1)
    mapGroups = map (parseMapGroup . tail) mapGroupStrs

sol :: [String] -> Int
sol ss = minimum $ map (applyAllMaps mgs) seeds
  where
    (seeds, mgs) = parseInput ss

-- CLI -------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 05!"
  putStrLn "Which puzzle would you like to solve (1):"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day05.txt" ReadMode
  raw <- hGetContents fHandle
  case (read puzNum) :: Int of
    1 -> print $ sol $ lines raw
    _ -> print "Invalid puzzle"
