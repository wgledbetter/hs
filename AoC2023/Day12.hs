module Day12 where

import Control.Parallel.Strategies
import Day10 (replaceInList)
import HB.Ch11 (idxsOf)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInput =
  [ "???.### 1,1,3",
    ".??..??...?##. 1,1,3",
    "?#?#?#?#?#?#?#? 1,3,1,6",
    "????.#...#... 4,1,1",
    "????.######..#####. 1,6,5",
    "?###???????? 3,2,1"
  ]

testOutput = 21

boolPerms :: Int -> [[Bool]]
boolPerms 0 = []
boolPerms 1 = concat $ foldr (\x y -> [b : x | b <- [False, True]] : y) [] [[]]
boolPerms n = concat $ foldr (\x y -> [b : x | b <- [False, True]] : y) [] $ boolPerms (n - 1)

allPerms :: [Char] -> [[Char]]
allPerms str = [perm f | f <- boolPerms $ length wildIdx]
  where
    wildIdx = idxsOf '?' str
    perm flags = foldr (\(wi, f) s -> replaceInList (wi, if f then '#' else '.') s) str $ zip wildIdx flags

-- numPerms :: [Char] -> Int
-- numPerms str = 2 ^ length $ idxsOf '?' str

groupsOfHash :: [Char] -> [Int]
groupsOfHash str = filter (/= 0) $ foldr (\c (g : gs) -> if c == '#' then (g + 1) : gs else 0 : g : gs) [0] str

groupsWithPattern :: [Int] -> [Char] -> [[Char]]
groupsWithPattern pat str = [p | p <- allPerms str, pat == groupsOfHash p]

numValidPerms :: [Int] -> [Char] -> Int
numValidPerms pat str = length $ groupsWithPattern pat str

parseInputLine :: String -> ([Char], [Int])
parseInputLine line = (head w, read ("[" ++ last w ++ "]") :: [Int])
  where
    w = words line

sol :: [String] -> Int
sol lines = sum parPerms
  where
    perms = map (uncurry (flip numValidPerms) . parseInputLine) lines
    parPerms = perms `using` parList rdeepseq

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 12!"
  putStrLn "Which puzzle would you like to solve (1)?"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day12.txt" ReadMode
  raw <- hGetContents fHandle
  case read puzNum :: Int of
    1 -> (print . sol . lines) raw
    _ -> print "Invalid puzzle"
