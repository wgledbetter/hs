module Day02 where

import HB.Ch09 (splitAndDrop)
import HB.Ch11 (capitalizeWord)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInputGames =
  [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  ]

testInputMaxes = (Red 12, Green 13, Blue 14)

testOutput = 8

-- sol :: [String] -> Int

data Cubes = Red Int | Green Int | Blue Int deriving (Eq, Show, Read)

instance Ord Cubes where
  compare (Red x) (Red y) = compare x y
  compare (Green x) (Green y) = compare x y
  compare (Blue x) (Blue y) = compare x y
  compare _ _ = error "Do not compare cubes of different colors"

data Game = Game Int [[Cubes]] deriving (Eq, Show)

countCubes :: Cubes -> Int
countCubes (Red n) = n
countCubes (Green n) = n
countCubes (Blue n) = n

parseCube :: String -> Cubes
parseCube str = read newStr
  where
    splitted = words str
    newStr = capitalizeWord $ splitted !! 1 ++ " " ++ splitted !! 0

parseGame :: String -> Game
parseGame gameStr = Game gameNum cubes
  where
    gameSplit = splitAndDrop ':' gameStr
    gameNum = read $ drop 5 $ gameSplit !! 0 :: Int
    drawStrs = splitAndDrop ';' $ gameSplit !! 1
    cubes = map (map parseCube) $ map (splitAndDrop ',') drawStrs

maxCubes :: Game -> (Cubes, Cubes, Cubes)
maxCubes (Game _ cubes) =
  foldr -- For each handful
    ( \cubeList currCounts ->
        foldr -- For each color in handful (effectively)
          ( \cube (r, g, b) -> case cube of
              -- If this handful has more red than the current max, update
              Red n -> if n > countCubes r then (Red n, g, b) else (r, g, b)
              -- If this handful has more green than the current max, update
              Green n -> if n > countCubes g then (r, Green n, b) else (r, g, b)
              -- If this handful has more blue than the current max, update
              Blue n -> if n > countCubes b then (r, g, Blue n) else (r, g, b)
          )
          currCounts -- Pass current draw totals to the inner counter
          cubeList -- The current draw
    )
    (Red 0, Green 0, Blue 0) -- Begin at zero
    cubes

gameIsPossible :: (Cubes, Cubes, Cubes) -> Game -> Bool
gameIsPossible (maxR, maxG, maxB) g = (actR <= maxR) && (actG <= maxG) && (actB <= maxB)
  where
    (actR, actG, actB) = maxCubes g

sol :: (Cubes, Cubes, Cubes) -> [String] -> Int
sol cubeMaxes stringGames = sum validGameNums
  where
    games = map parseGame stringGames
    validGames = filter (gameIsPossible cubeMaxes) games
    validGameNums = map (\(Game gn _) -> gn) validGames

-- Puz 2 -----------------------------------------------------------------------

test2In = testInputGames

test2Out = 2286

cubePower :: (Cubes, Cubes, Cubes) -> Int
cubePower (a, b, c) = (countCubes a) * (countCubes b) * (countCubes c)

sol2 :: [String] -> Int
sol2 = sum . (map (cubePower . maxCubes . parseGame))

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 02!"
  putStrLn "Which puzzle would you like to solve? (1):"
  puzNum <- getLine
  gfHandle <- openFile "AoC2023/input/Day02-games.txt" ReadMode
  mfHandle <- openFile "AoC2023/input/Day02-maxes.txt" ReadMode
  rawGames <- hGetContents gfHandle
  rawMaxes <- hGetContents mfHandle
  case (read puzNum) :: Int of
    1 -> print $ sol ((read rawMaxes) :: (Cubes, Cubes, Cubes)) (lines rawGames)
    2 -> print $ sol2 $ lines rawGames
    _ -> print "Invalid puzzle"
