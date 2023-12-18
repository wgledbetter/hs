module Day13 where

import Data.List (sortBy)
import Day08 (transpose)
import HB.Ch09 (isEmpty, splitAndDrop)
import HB.Ch11 (contains)
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
arrayValue = reflValue . arrayRefls

parseInput :: [String] -> [[String]]
parseInput = splitAndDrop ""

sol :: [String] -> Int
sol = sum . (map arrayValue) . parseInput

-- Puz 2 -----------------------------------------------------------------------

intersection' :: (Eq a) => [a] -> [a] -> [a]
intersection' l1 = foldr (\l2Item accum -> if contains l2Item l1 then l2Item : accum else accum) []

intersection :: (Eq a) => [[a]] -> [a]
intersection (l : []) = l
intersection (l1 : l2 : []) = intersection' l1 l2
intersection (l : ls) = intersection (l : [intersection ls])

everyoneElseHas :: (Eq a) => [a] -> [[a]] -> [a]
everyoneElseHas lRef ee =
  foldr
    (\allOthersItem accum -> if contains allOthersItem lRef then accum else allOthersItem : accum)
    []
    $ intersection ee

dropVal :: (Eq a) => a -> [a] -> [a]
dropVal _ [] = []
dropVal v (x : xs) = if v == x then dropVal v xs else x : dropVal v xs

dropIdx :: Int -> [a] -> [a]
dropIdx 0 (l : ls) = ls
dropIdx n (x : xs) = x : dropIdx (n - 1) xs

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : xs) = Just x

-- Find the index of the list that doesn't have something that everyone else has.
-- STRONG ASSUMPTION THAT:
--   1. Only one list will have uncommon values
--   2. Only one thing will be uncommon in that list
findUncommon :: (Eq a) => [[a]] -> (Int, Maybe a)
findUncommon lists = case maybeUncommon of
  Just (id, unComList) -> (id, maybeHead unComList)
  Nothing -> (0, Nothing)
  where
    eeh = map (\(id, l) -> (id, everyoneElseHas l (dropIdx id lists))) $ zip [0 ..] lists
    maybeUncommon = maybeHead $ filter (\(id, l) -> not $ isEmpty l) eeh

allEndedRefls :: (Eq a) => [[a]] -> [[(Int, Int)]]
allEndedRefls = map reflsWithEnd

reflValue :: ArrayRefls -> Int
reflValue (ArrayRefls rr cr) = rVal + cVal
  where
    rVal = foldr (\rf val -> val + 100 * nBeforeRefl rf) 0 rr
    cVal = foldr (\rf val -> val + nBeforeRefl rf) 0 cr

unMaybeToList :: Maybe a -> [a]
unMaybeToList ml = case ml of
  Just x -> [x]
  Nothing -> []

unSmudgedValue :: (Eq a) => [[a]] -> Int
unSmudgedValue arr = reflValue smudgeFixes
  where
    (_, maybeColUnc) = findUncommon $ allEndedRefls arr
    (_, maybeRowUnc) = findUncommon $ allEndedRefls $ transpose arr
    smudgeFixes = ArrayRefls {rowRefls = unMaybeToList maybeRowUnc, colRefls = unMaybeToList maybeColUnc}

sol2 :: [String] -> Int
sol2 = sum . (map unSmudgedValue) . parseInput

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
    2 -> (print . sol2 . lines) raw
    _ -> print "Invalid puzzle."
