module Day04 where

import HB.Ch09 (splitAndDrop)
import HB.Ch11 (contains)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInput =
  [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  ]

testOutput = 13

data Card = Card [Int] [Int] deriving (Eq, Show)

cardValue :: Card -> Int
cardValue (Card winning have) =
  if length haveWinning > 0
    then 2 ^ (length haveWinning - 1)
    else 0
  where
    haveWinning = [x | x <- have, contains x winning]

parseCard :: String -> Card
parseCard cs = Card (map read winning) (map read have)
  where
    nums = splitAndDrop ':' cs !! 1
    splitNums = splitAndDrop '|' nums
    winning = words $ head splitNums
    have = words $ (splitNums !! 1)

sol :: [String] -> Int
sol = sum . map (cardValue . parseCard)

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 04!"
  putStrLn "Which puzzle would you like to solve? (1):"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day04.txt" ReadMode
  raw <- hGetContents fHandle
  case (read puzNum) :: Int of
    1 -> print $ sol $ lines raw
    _ -> print "Invalid puzzle"
