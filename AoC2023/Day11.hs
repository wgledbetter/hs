module Day11 where

import Day08 (transpose)
import HB.Ch11 (contains)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInput =
  [ "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#....."
  ]

testOutput = 374

addRowOf :: Int -> a -> [[a]] -> [[a]]
addRowOf at val orig = go 0 orig
  where
    go thisIdx (r : rs) =
      if thisIdx == at
        then r : (take (length r) $ repeat val) : rs
        else r : go (thisIdx + 1) rs

addColOf :: Int -> a -> [[a]] -> [[a]]
addColOf at val orig = transpose $ addRowOf at val $ transpose orig

expand :: [[Char]] -> [[Char]]
expand universe = fullyExpanded
  where
    emptyRows = map fst $ filter (\(rIdx, r) -> not $ contains '#' r) $ zip [0 ..] universe
    emptyCols = map fst $ filter (\(cIdx, c) -> not $ contains '#' c) $ zip [0 ..] $ transpose universe
    addedRows = foldr (\i u -> addRowOf i '.' u) universe emptyRows
    fullyExpanded = foldr (\i u -> addColOf i '.' u) addedRows emptyCols

findGalaxies :: [[Char]] -> [(Int, Int)]
findGalaxies universe =
  concatMap (\(rIdx, r) -> galaxiesInRow rIdx r) $ zip [0 ..] universe
  where
    galaxiesInRow i r =
      foldr
        ( \(cIdx, theChar) l ->
            if theChar == '#'
              then (i, cIdx) : l
              else l
        )
        []
        $ zip [0 ..] r

-- taxicab norm
distBetween :: (Int, Int) -> (Int, Int) -> Int
distBetween (r1, c1) (r2, c2) = abs (r2 - r1) + abs (c2 - c1)

sol :: [String] -> Int
sol universe = sum [distBetween a b | (i, a) <- zip [0 ..] galaxies, b <- drop (i + 1) galaxies]
  where
    expandedUniverse = expand universe
    galaxies = findGalaxies expandedUniverse

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 11!"
  putStrLn "Which puzzle would you like to solve (1)?"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day11.txt" ReadMode
  raw <- hGetContents fHandle
  case (read puzNum) :: Int of
    1 -> (print . sol . lines) raw
    _ -> print "Invalid puzzle"
