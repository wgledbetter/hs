module Day10 where

import HB.Ch09 (isEmpty)
import HB.Ch11 (contains)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInput1 =
  [ ".....",
    ".S-7.",
    ".|.|.",
    ".L-J.",
    "....."
  ]

testOutput1 = 4

testInput2 =
  [ "..F7.",
    ".FJ|.",
    "SJ.L7",
    "|F--J",
    "LJ..."
  ]

testOutput2 = 8

myMaze =
  [ ".....",
    "..|..",
    ".-S-.",
    "..|..",
    "....."
  ]

charAt :: [[Char]] -> (Int, Int) -> Char
charAt maze (rIdx, cIdx) = (maze !! rIdx) !! cIdx

connectionDirections :: Char -> [(Int, Int)]
connectionDirections '.' = []
connectionDirections 'S' = [(-1, 0), (0, 1), (1, 0), (0, -1)]
connectionDirections '|' = [(-1, 0), (1, 0)]
connectionDirections '-' = [(0, -1), (0, 1)]
connectionDirections 'L' = [(-1, 0), (0, 1)]
connectionDirections 'J' = [(-1, 0), (0, -1)]
connectionDirections '7' = [(1, 0), (0, -1)]
connectionDirections 'F' = [(1, 0), (0, 1)]

connectsTo :: [[Char]] -> (Int, Int) -> [(Int, Int)]
connectsTo maze idx@(rC, cC) =
  map (\(c, (x, y)) -> (rC + x, cC + y)) $
    filter
      -- Check that the candidate connection connects back to us
      (\(c, (rConDir, cConDir)) -> contains (-rConDir, -cConDir) $ connectionDirections c)
      candidateConns
  where
    thisChar = charAt maze idx
    connDirs = connectionDirections thisChar
    candidateConns =
      map (\(x, y) -> (charAt maze (rC + x, cC + y), (x, y))) $
        filter
          ( \(x, y) ->
              (rC + x >= 0) && (cC + y >= 0) && (rC + x < length maze) && (cC + y < (length . head) maze)
          )
          connDirs

stepPath :: [[Char]] -> [(Int, Int)] -> [[(Int, Int)]]
stepPath maze path = [op : path | op <- realOptions]
  where
    here = head path
    options = connectsTo maze here
    realOptions =
      filter
        ( \op ->
            (op == last path && length path > 2) || (not $ contains op path)
        )
        options

findLoop :: [[Char]] -> (Int, Int) -> [(Int, Int)]
findLoop maze start = go $ stepPath maze [start]
  where
    go paths =
      if not $ isEmpty pathsThatEndAtStart
        then head pathsThatEndAtStart
        else go steppedPaths
      where
        steppedPaths = filter (not . isEmpty) $ concatMap (stepPath maze) paths
        pathsThatEndAtStart = filter (\path -> head path == start) steppedPaths

getStart :: [[Char]] -> (Int, Int)
getStart maze = (rIdx, cIdx)
  where
    rowsWithSIndex = map (\mazeRow -> filter (\(cIdx, char) -> char == 'S') $ zip [0 ..] mazeRow) maze
    sRow = filter (\(rIdx, row) -> not $ isEmpty row) $ zip [0 ..] rowsWithSIndex
    rIdx = fst $ head sRow
    cIdx = fst $ head $ snd $ head sRow

simplifyMaze :: [[Char]] -> [[Char]]
simplifyMaze maze =
  map
    ( \(rIdx, row) ->
        map
          ( \(cIdx, mazeItem) ->
              if 2 > (length $ connectsTo maze (rIdx, cIdx))
                then '.'
                else mazeItem
          )
          $ zip [0 ..] row
    )
    $ zip [0 ..] maze

fixa :: (a -> a -> Bool) -> (a -> a) -> a -> Int -> a
fixa ok iter z 0 = z
fixa ok iter z n = loop z n
  where
    loop z n =
      let next = iter z
       in if (ok next z)
            then next
            else fixa ok iter next (n - 1)

reduceMaze :: [[Char]] -> Int -> [[Char]]
reduceMaze = fixa (==) simplifyMaze

sol :: [String] -> Double
sol maze = (fromIntegral (length $ findLoop maze $ getStart maze) - 1.0) / 2.0

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 10!"
  putStrLn "Which puzzle would you like to solve (1)?"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day10.txt" ReadMode
  raw <- hGetContents fHandle
  case (read puzNum) :: Int of
    1 -> (print . sol . lines) raw
    _ -> print "Invalid puzzle"
