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

-- Puz 2 -----------------------------------------------------------------------

test2Input1 =
  [ "...........",
    ".S-------7.",
    ".|F-----7|.",
    ".||.....||.",
    ".||.....||.",
    ".|L-7.F-J|.",
    ".|..|.|..|.",
    ".L--J.L--J.",
    "..........."
  ]

test2Output1 = 4

test2Input2 =
  [ ".F----7F7F7F7F-7....",
    ".|F--7||||||||FJ....",
    ".||.FJ||||||||L7....",
    "FJL7L7LJLJ||LJ.L-7..",
    "L--J.L7...LJS7F-7L7.",
    "....F-J..F7FJ|L7L7L7",
    "....L7.F7||L7|.L7L7|",
    ".....|FJLJ|FJ|F7|.LJ",
    "....FJL-7.||.||||...",
    "....L---J.LJ.LJLJ..."
  ]

test2Output2 = 8

test2Input3 =
  [ "FF7FSF7F7F7F7F7F---7",
    "L|LJ||||||||||||F--J",
    "FL-7LJLJ||||||LJL-77",
    "F--JF--7||LJLJ7F7FJ-",
    "L---JF-JLJ.||-FJLJJ7",
    "|F|F-JF---7F7-L7L|7|",
    "|FFJF7L7F-JF7|JL---7",
    "7-L-JL7||F7|L7F-7F7|",
    "L.L7LFJ|||||FJL7||LJ",
    "L7JLJL-JLJLJL--JLJ.L"
  ]

test2Output3 = 10

data Sense = CW | CCW | Straight deriving (Eq, Show)

data TopBottom = Top | Bottom deriving (Eq, Show)

data State
  = Outside
  | Inside
  | Entering
  | Exiting
  | BorderFromOut TopBottom
  | BorderFromIn TopBottom
  deriving (Eq, Show)

rowStates :: [[Char]] -> [(Int, Int)] -> (Int, [Char]) -> [State]
rowStates maze path (rIdx, row) =
  tail
    $ scanl
      ( \currState (cIdx, thisChar) ->
          if contains (rIdx, cIdx) path
            then case thisChar of
              '|' -> case currState of
                Outside -> Entering
                Inside -> Exiting
                Entering -> Exiting
                Exiting -> Entering
              '-' -> currState
              'L' -> case currState of
                Outside -> BorderFromOut Top
                Inside -> BorderFromIn Top
                Exiting -> BorderFromOut Top
                Entering -> BorderFromIn Top
              'J' -> case currState of
                BorderFromIn Top -> Entering
                BorderFromIn Bottom -> Exiting
                BorderFromOut Top -> Exiting
                BorderFromOut Bottom -> Entering
              '7' -> case currState of
                BorderFromIn Top -> Exiting
                BorderFromIn Bottom -> Entering
                BorderFromOut Top -> Entering
                BorderFromOut Bottom -> Exiting
              'F' -> case currState of
                Outside -> BorderFromOut Bottom
                Inside -> BorderFromIn Bottom
                Entering -> BorderFromIn Bottom
                Exiting -> BorderFromOut Bottom
            else case currState of
              Inside -> Inside
              Outside -> Outside
              Entering -> Inside
              Exiting -> Outside
      )
      Outside
    $ zip (enumFrom 0) row

countInRow :: [[Char]] -> [(Int, Int)] -> (Int, [Char]) -> Int
countInRow maze path (rIdx, row) = length $ filter (== Inside) $ rowStates maze path (rIdx, row)

countWithin :: [[Char]] -> [(Int, Int)] -> Int
countWithin maze path = sum $ map (countInRow maze path) $ zip (enumFrom 0) maze

replaceInList :: (Int, a) -> [a] -> [a]
replaceInList (idx, item) l = go 0 l
  where
    go currIdx (x : xs) = if currIdx == idx then item : xs else x : (go (currIdx + 1) xs)

swapStart :: [[Char]] -> [[Char]]
swapStart maze = replaceInList (sr, replaceInList (sc, startChar) (maze !! sr)) maze
  where
    start@(sr, sc) = getStart maze
    startConns = connectsTo maze start
    connDirs = map (\(cr, cc) -> (cr - sr, cc - sc)) startConns
    startChar =
      if contains (1, 0) connDirs
        then case filter (/= (1, 0)) connDirs of
          [(-1, 0)] -> '|'
          [(0, 1)] -> 'F'
          [(0, -1)] -> '7'
        else
          if contains (-1, 0) connDirs
            then case filter (/= (-1, 0)) connDirs of
              [(1, 0)] -> '|'
              [(0, 1)] -> 'L'
              [(0, -1)] -> 'J'
            else
              if contains (0, 1) connDirs
                then case filter (/= (0, 1)) connDirs of
                  [(-1, 0)] -> 'L'
                  [(1, 0)] -> 'F'
                  [(0, -1)] -> '-'
                else
                  if contains (0, -1) connDirs
                    then case filter (/= (0, -1)) connDirs of
                      [(-1, 0)] -> 'J'
                      [(1, 0)] -> '7'
                      [(0, 1)] -> '-'
                    else 'S'

sol2 :: [String] -> Int
sol2 maze = countWithin (swapStart maze) (findLoop maze (getStart maze))

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 10!"
  putStrLn "Which puzzle would you like to solve (1 or 2)?"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day10.txt" ReadMode
  raw <- hGetContents fHandle
  case (read puzNum) :: Int of
    1 -> (print . sol . lines) raw
    2 -> (print . sol2 . lines) raw
    _ -> print "Invalid puzzle"
