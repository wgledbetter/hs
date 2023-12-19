module Day14 where

import Data.List (sort)
import Day08 (transpose)
import Day13 (intersection')
import HB.Ch09 (isEmpty, splitAndDrop)
import HB.Ch11 (contains, idxsOf)
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

-- Puz 2 -----------------------------------------------------------------------

arrIdxsOf :: (Eq a) => a -> [[a]] -> [(Int, Int)]
arrIdxsOf item arr = concatMap (\(r, l) -> map (\i -> (r, i)) l) $ filter (not . isEmpty . snd) temp
  where
    temp = map (\(r, i) -> (r, idxsOf item i)) $ zip [0 ..] arr

toCoords :: [String] -> ([(Int, Int)], [(Int, Int)])
toCoords arr = (stationary, mobile)
  where
    stationary = arrIdxsOf '#' arr
    mobile = arrIdxsOf 'O' arr

data Direction = North | West | South | East deriving (Eq, Show)

isBetween :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isBetween (x1, y1) (x2, y2) (i, j) =
  (min x1 x2 <= i)
    && (i <= max x1 x2)
    && (min y1 y2 <= j)
    && (j <= max y1 y2)

tupleSum :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

negTuple :: (Int, Int) -> (Int, Int)
negTuple (x, y) = (-x, -y)

splitAndDropIf :: (a -> Bool) -> [a] -> [[a]]
splitAndDropIf _ [] = []
splitAndDropIf cond (x : xs)
  | cond x = splitAndDropIf cond xs
  | otherwise = takeWhile (not . cond) (x : xs) : splitAndDropIf cond (dropWhile (not . cond) xs)

-- "Tilt" a list towards the front
tiltList :: Int -> [Int] -> [Int] -> [Int]
tiltList size static mobile = newMobile
  where
    idxGroups = splitAndDropIf (flip contains static) [0 .. size - 1]
    mobilePer = map (length . intersection' mobile) idxGroups
    newMobile = concatMap (\(n, ig) -> take n $ enumFrom $ head ig) $ zip mobilePer idxGroups

tiltListBack :: Int -> [Int] -> [Int] -> [Int]
tiltListBack size static mobile = cvt $ tiltList size (cvt static) (cvt mobile)
  where
    cvt' i = size - i - 1
    cvt = map cvt'

tilt :: (Int, Int) -> [(Int, Int)] -> Direction -> [(Int, Int)] -> [(Int, Int)]
tilt (nR, nC) static dir mobile = case dir of
  North ->
    concatMap
      ( \ci ->
          map (\newRow -> (newRow, ci)) $
            tiltList nR (map fst $ filter (\(sr, sc) -> sc == ci) static) (map fst $ filter (\(mr, mc) -> mc == ci) mobile)
      )
      [0 .. nC - 1]
  South ->
    concatMap
      ( \ci ->
          map (\newRow -> (newRow, ci)) $
            tiltListBack nR (map fst $ filter (\(sr, sc) -> sc == ci) static) (map fst $ filter (\(mr, mc) -> mc == ci) mobile)
      )
      [0 .. nC - 1]
  East ->
    concatMap
      ( \ri ->
          map (\newCol -> (ri, newCol)) $
            tiltListBack nC (map snd $ filter (\(sr, sc) -> sr == ri) static) (map snd $ filter (\(mr, mc) -> mr == ri) mobile)
      )
      [0 .. nR - 1]
  West ->
    concatMap
      ( \ri ->
          map (\newCol -> (ri, newCol)) $
            tiltList nC (map snd $ filter (\(sr, sc) -> sr == ri) static) (map snd $ filter (\(mr, mc) -> mr == ri) mobile)
      )
      [0 .. nR - 1]

spin :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
spin dims static = sort . (tt East . tt South . tt West . tt North)
  where
    tt = tilt dims static

cyclone :: (Int, Int) -> [(Int, Int)] -> [((Int, Int))] -> [[(Int, Int)]]
cyclone dims static mobile = mobile : cyclone dims static (spin dims static mobile)

-- Designed for infinite lists
takeUntilRepeat :: (Eq a) => [a] -> [a]
takeUntilRepeat l = base ++ [l !! length base]
  where
    base = foldl (\l i -> if contains i l then l else l ++ [i]) [] l

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
