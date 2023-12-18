module Day05 where

import Data.List (sort, sortBy)
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

-- Puz 2 -----------------------------------------------------------------------

test2Output = 46

type Range = (Int, Int)

sortRanges :: [Range] -> [Range]
sortRanges = sortBy (\(s1, _) (s2, _) -> compare s1 s2)

simplifyRanges :: [Range] -> [Range]
simplifyRanges rs = combine sorted
  where
    sorted = sortRanges rs
    combine [] = []
    combine (r : []) = [r]
    combine (r1@(s1, l1) : r2@(s2, l2) : rr) =
      if s1 + l1 >= s2
        then combine ((s1, max (s1 + l1) (s2 + l2) - s1) : rr)
        else r1 : combine (r2 : rr)

rangeInputOf :: Map -> Range
rangeInputOf (Map _ ss r) = (ss, r)

rangeOverlap :: Range -> Range -> Maybe Range
rangeOverlap r1@(s1, l1) r2@(s2, l2) = if l > 0 then Just (s, l) else Nothing
  where
    p1 = s1 + l1
    p2 = s2 + l2
    s = max s1 s2
    l = min p1 p2 - s

-- Portion of map input that overlaps with given range
mapOverlap :: Map -> Range -> Maybe Range
mapOverlap m = rangeOverlap (rangeInputOf m)

-- Map the given range
mapRange :: Map -> Range -> Range
mapRange m r@(s, l) = (performMap m s, l)

subRange :: Range -> Range -> [Range]
subRange r1@(s1, l1) r2@(s2, l2)
  | overlap == Nothing = [r1]
  | otherwise = remove overlap r1
  where
    overlap = rangeOverlap r1 r2
    remove (Just ov@(so, lo)) r@(s, l) =
      if so == s -- If the overlap starts at the beginning
        then
          ( if lo == l -- If the overlap covers the whole range
              then [] -- Nothing remains
              else [(s + lo, l - lo)] -- The back portion remains
          )
        else
          ( if so + lo == s + l -- If the overrlap covers the end
              then [(s, l - lo)] -- Start is same, just shorter
              else [(s, so - s), (so + lo, s + l - (so + lo))] -- Overlap is central
          )

-- Find maps that overlap
-- Find individual ranges to pass to each map
-- Simplify ranges
groupMapRange :: MapGroup -> Range -> [Range]
groupMapRange mg r@(s, l) = simplifyRanges $ notMapped ++ map (uncurry mapRange) validMaps
  where
    mapOverlapRange = map (\m -> (m, mapOverlap m r)) mg
    validMaps =
      map
        (\(m, (Just mo)) -> (m, mo))
        $ filter (\(m, mo) -> mo /= Nothing) mapOverlapRange
    notMapped =
      foldr
        (\(_, ovl) ranges -> concat $ map (\r -> subRange r ovl) ranges)
        [r]
        validMaps

applyAllRangeMapGroups :: [MapGroup] -> Range -> [Range]
applyAllRangeMapGroups mgs r =
  foldl
    ( \rs mg ->
        simplifyRanges $ concatMap (groupMapRange mg) rs
    )
    [r]
    mgs

toRanges :: [Int] -> [Range]
toRanges [] = []
toRanges (a : b : rest) = (a, b) : toRanges rest

parseInput2 :: [String] -> ([Range], [MapGroup])
parseInput2 ss = (toRanges seeds, mgs)
  where
    (seeds, mgs) = parseInput ss

sol2 :: [String] -> Int
sol2 ss = fst $ minimum $ concatMap (applyAllRangeMapGroups mgs) seedRanges
  where
    (seedRanges, mgs) = parseInput2 ss

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
    2 -> print $ sol2 $ lines raw
    _ -> print "Invalid puzzle"
