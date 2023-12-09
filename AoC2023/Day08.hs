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

listifyWithSteps :: String -> BinaryTree a -> [a]
listifyWithSteps steps tree = go (concat $ repeat steps) tree
  where
    go _ Leaf = []
    go (s : ss) (Node lt c rt) = case s of
      'L' -> c : go ss lt
      'R' -> c : go ss rt
      _ -> error $ "Incorrect direction indicator: " ++ [s]

pathTo :: (Eq a) => a -> String -> BinaryTree a -> [a]
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

type Branch = (String, (String, String))

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

connectionsToNode :: [Branch] -> String -> [(String, Char)]
connectionsToNode branches node =
  foldr
    ( \(bNode, (bLeft, bRight)) l ->
        let leftEq = bLeft == node
            rightEq = bRight == node
         in if leftEq && rightEq
              then (bNode, 'L') : (bNode, 'R') : l
              else
                if leftEq
                  then (bNode, 'L') : l
                  else
                    if rightEq
                      then (bNode, 'R') : l
                      else l
    )
    []
    branches

-- Find items that are on left and right of branch definition
-- Return type is [(Node that cycles, (node that cycles to this node, direction from node))]
cycles :: [Branch] -> [(String, [(String, Char)])]
cycles branches =
  map (\n -> (n, connectionsToNode branches n)) nodes
  where
    nodes = map fst branches

beenHereBefore :: [(Int, Char, String)] -> [Bool]
beenHereBefore l = False : go (take 1 l) (drop 1 l)
  where
    go already (x : xs) = (contains x already) : go (already ++ [x]) xs

firstLoopOf :: String -> BinaryTree String -> [(Int, Char, String)]
firstLoopOf steps tree = take (nUntilLoop + 1) idStepNode
  where
    stepList = listifyWithSteps steps tree
    stepCounts = take (length steps) [0 ..]
    idStepNode = zip3 (concat $ repeat stepCounts) (concat $ repeat steps) stepList
    nUntilLoop = length $ takeWhile (== False) $ beenHereBefore idStepNode

loopOffset :: [(Int, Char, String)] -> Int
loopOffset loop = length $ takeWhile (/= last loop) loop

loopLength :: [(Int, Char, String)] -> Int
loopLength loop = (+) (-1) $ length $ drop (loopOffset loop) loop

loopPattern :: (String -> Bool) -> [(Int, Char, String)] -> ([Int], [Int])
loopPattern detect loop = (idxsOf True preLoopDetects, idxsOf True loopDetects)
  where
    nPreLoop = loopOffset loop
    nLoop = loopLength loop
    preLoopDetects = map (\(i, c, s) -> detect s) $ take nPreLoop loop
    loopDetects = map (\(i, c, s) -> detect s) $ init $ drop nPreLoop loop

occurancesOf :: (String -> Bool) -> String -> BinaryTree String -> [Int]
occurancesOf detect steps tree =
  preLoopIdxs ++ [lOff + lc * lLen + inLoopIdx | lc <- [0 ..], inLoopIdx <- inLoopIdxs]
  where
    loop = firstLoopOf steps tree
    lOff = loopOffset loop
    lLen = loopLength loop
    (preLoopIdxs, inLoopIdxs) = loopPattern detect loop

specialContains :: (Ord a) => a -> [a] -> Bool
specialContains _ [] = False
specialContains val (x : xs)
  | val == x = True
  | val < x = False
  | otherwise = specialContains val xs

lowestInAll :: (Ord a) => [[a]] -> a
lowestInAll (l : ls) =
  head $
    filter
      (\candidate -> all (\ll -> specialContains candidate ll) ls)
      l

-- bad name. does not generalize. relies on the fact that our condition happens to only occur once per loop.
commonFrequency :: (Enum a, Ord a) => [a] -> [a] -> [a]
commonFrequency l1 l2 = enumFromThen c1 c2
  where
    c1 = lowestInAll [l1, l2]
    x1 = length $ takeWhile (<= c1) l1
    x2 = length $ takeWhile (<= c1) l2
    c2 = lowestInAll [drop x1 l1, drop x2 l2]

lowestInAll' :: (Ord a, Enum a) => [[a]] -> a
lowestInAll' (l : []) = head l
lowestInAll' (l1 : l2 : ls) = lowestInAll' $ (commonFrequency l1 l2) : ls

-- Returns list of (b,m) such that idx = b + m*loopCount
equationsOf :: (String -> Bool) -> String -> BinaryTree String -> [(Int, Int)]
equationsOf detect steps tree = [(x, 0) | x <- preLoopIdxs] ++ [(lOff + x, lLen) | x <- inLoopIdxs]
  where
    loop = firstLoopOf steps tree
    lOff = loopOffset loop
    lLen = loopLength loop
    (preLoopIdxs, inLoopIdxs) = loopPattern detect loop

genFromEquation :: (Int, Int) -> [Int]
genFromEquation (b, m) = [b + m * i | i <- [0 ..]]

genFromEquations :: [(Int, Int)] -> [Int]
genFromEquations eqs = [b + m * i | i <- [0 ..], (b, m) <- eqs]

equationContains :: Int -> (Int, Int) -> Bool
equationContains v (b, m) = rem (v - b) m == 0

equationsContain :: Int -> [(Int, Int)] -> Bool
equationsContain v eqs = any (\eq -> equationContains v eq) eqs

lowestCommonInEqs :: [[(Int, Int)]] -> Int
lowestCommonInEqs (eq : eqs) =
  head $
    filter (\candidate -> all (\e -> equationsContain candidate e) eqs) $
      genFromEquations eq

firstCommonOccurance :: (String -> Bool) -> String -> [BinaryTree String] -> Int
firstCommonOccurance detect steps trees = lowestInAll occurances
  where
    occurances = map (\t -> occurancesOf detect steps t) trees

firstCommonOccurance' :: (String -> Bool) -> String -> [BinaryTree String] -> Int
firstCommonOccurance' detect steps trees = lowestCommonInEqs eqs
  where
    eqs = map (\t -> equationsOf detect steps t) trees

sol2 :: [String] -> Int
sol2 ls = length pathsToAllZs
  where
    (steps, branches) = parseInput ls
    nodes = map fst branches
    trees = map (\startNode -> unfoldTree startNode branches) $ startNodes nodes -- A list of all trees beginning at each start node
    paths = map (\t -> listifyWithSteps steps t) trees -- Listification of all trees using the given directions
    pathSteps = transpose paths -- Regroup paths into individual steps
    pathsToAllZs = takeWhile (\ps -> any (\p -> last p /= 'Z') ps) pathSteps -- Stop paths when everything ends in Z.

sol2' :: [String] -> Int
sol2' ls = firstCommonOccurance' (\s -> last s == 'Z') steps trees
  where
    (steps, branches) = parseInput ls
    nodes = map fst branches
    trees = map (\startNode -> unfoldTree startNode branches) $ startNodes nodes

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
    2 -> (print . sol2' . lines) raw
    _ -> print "Invalid puzzle"
