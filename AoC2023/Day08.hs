module Day08 where

import Data.List (find)
import HB.Ch09 (isEmpty, splitAndDrop)
import HB.Ch11 (BinaryTree (Leaf, Node), contains)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInput1 =
  [ "RL",
    "",
    "AAA = (BBB, CCC)",
    "BBB = (DDD, EEE)",
    "CCC = (ZZZ, GGG)",
    "DDD = (DDD, DDD)",
    "EEE = (EEE, EEE)",
    "GGG = (GGG, GGG)",
    "ZZZ = (ZZZ, ZZZ)"
  ]

testOutput1 = 2

testInput2 =
  [ "LLR",
    "",
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)"
  ]

testOutput2 = 6

unfoldTree :: String -> [(String, (String, String))] -> BinaryTree String
unfoldTree start branches = case generate start of
  Just (left, center, right) -> Node (unfoldTree left branches) center (unfoldTree right branches)
  Nothing -> Node Leaf start Leaf
  where
    generate x = case find (\(n, (l, r)) -> n == x) branches of
      Just (node, (l, r)) -> Just (l, node, r)
      Nothing -> Nothing

listifyWithSteps :: String -> BinaryTree String -> [String]
listifyWithSteps steps tree = go (concat $ repeat steps) tree
  where
    go _ Leaf = []
    go (s : ss) (Node lt c rt) = case s of
      'L' -> c : go ss lt
      'R' -> c : go ss rt
      _ -> error $ "Incorrect direction indicator: " ++ [s]

pathTo :: String -> String -> BinaryTree String -> [String]
pathTo end steps tree = takeWhile (/= end) $ listifyWithSteps steps tree

parseBranch :: String -> (String, (String, String))
parseBranch bs = (node, (l, r))
  where
    nodeLR = splitAndDrop '=' bs
    node = takeWhile (/= ' ') $ head nodeLR
    lrStr = drop 1 $ last nodeLR
    l = take 3 $ drop 1 lrStr
    r = take 3 $ drop 6 lrStr

parseInput :: [String] -> (String, [(String, (String, String))])
parseInput ls = (dirs, map parseBranch branchStrs)
  where
    dirs = head ls
    branchStrs = drop 2 ls

sol :: [String] -> Int
sol ls = length $ pathTo "ZZZ" steps tree
  where
    (steps, branches) = parseInput ls
    tree = unfoldTree "AAA" branches

-- Puz 2 -----------------------------------------------------------------------

test2Input =
  [ "LR",
    "",
    "11A = (11B, XXX)",
    "11B = (XXX, 11Z)",
    "11Z = (11B, XXX)",
    "22A = (22B, XXX)",
    "22B = (22C, 22C)",
    "22C = (22Z, 22Z)",
    "22Z = (22B, 22B)",
    "XXX = (XXX, XXX)"
  ]

test2Output = 6

startNodes :: [String] -> [String]
startNodes = filter (\s -> 'A' == last s)

endNodes :: [String] -> [String]
endNodes = filter (\s -> 'Z' == last s)

transpose :: [[a]] -> [[a]]
transpose ls =
  if any isEmpty ls
    then []
    else map head ls : transpose (map (drop 1) ls)

sol2 :: [String] -> Int
sol2 ls = length pathsToAllZs
  where
    (steps, branches) = parseInput ls
    nodes = map fst branches
    trees = map (\startNode -> unfoldTree startNode branches) $ startNodes nodes -- A list of all trees beginning at each start node
    paths = map (\t -> listifyWithSteps steps t) trees -- Listification of all trees using the given directions
    pathSteps = transpose paths -- Regroup paths into individual steps
    pathsToAllZs = takeWhile (\ps -> any (\p -> last p /= 'Z') ps) pathSteps -- Stop paths when everything ends in Z.

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 08!"
  putStrLn "Which puzzle would you like to solve (1 or 2)?"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day08.txt" ReadMode
  raw <- hGetContents fHandle
  case (read puzNum) :: Int of
    1 -> (print . sol . lines) raw
    2 -> (print . sol2 . lines) raw
    _ -> print "Invalid puzzle"
