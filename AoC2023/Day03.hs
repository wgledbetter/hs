module Day03 where

import HB.Ch11 (contains)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInput =
  [ "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  ]

testOutput = 4361

data Number = Number
  { row :: Int,
    col :: Int,
    size :: Int,
    value :: Int
  }
  deriving (Eq, Show)

data Symbol = Symbol Int Int Char deriving (Eq, Show)

-- Find contiguous subsequence consisting of a set of values
subseqsContaining :: (Eq a) => [a] -> [a] -> [(Int, Int)]
subseqsContaining allowed input = go 0 allowed input
  where
    isAllowed c = contains c allowed
    go _ _ [] = []
    go offset valid ref =
      if numTook > 0
        then (offset, numTook) : go (offset + numTook) valid (drop numTook ref)
        else go (offset + numNotTook) valid (drop numNotTook ref)
      where
        took = takeWhile isAllowed ref
        notTook = takeWhile (not . isAllowed) ref
        numTook = length took
        numNotTook = length notTook

subseqsOfDigits :: String -> [(Int, Int)]
subseqsOfDigits = subseqsContaining "0123456789"

getNumbers :: [String] -> [Number]
getNumbers ll =
  concatMap
    ( \(lineNum, theLine) ->
        map
          ( \(idx, sz) ->
              Number {row = lineNum, col = idx, size = sz, value = read (take sz $ drop idx theLine)}
          )
          $ subseqsOfDigits theLine
    )
    (zip (enumFrom 0) ll)

isSymbol :: Char -> Bool
isSymbol = not . (flip contains ".0123456789")

getSymbols :: [String] -> [Symbol]
getSymbols ll =
  concatMap
    ( \(lineNum, theLine) ->
        foldr
          ( \(colNum, theChar) theSymbols ->
              if isSymbol theChar
                then Symbol lineNum colNum theChar : theSymbols
                else theSymbols
          )
          []
          (zip (enumFrom 0) theLine)
    )
    (zip (enumFrom 0) ll)

isAdjacent :: Symbol -> Number -> Bool
isAdjacent (Symbol sr sc _) (Number nr nc ns _) =
  ((nr - 1) <= sr && sr <= (nr + 1)) -- Row is within 1
    && ((nc - 1) <= sc && (sc <= (nc + ns))) -- Col is within 1 of full size

unique :: (Eq a) => [a] -> [a]
unique = foldr (\item existing -> if contains item existing then existing else item : existing) []

getPartNumbers :: [Number] -> [Symbol] -> [Number]
getPartNumbers numbers symbols = unique [n | n <- numbers, s <- symbols, isAdjacent s n]

sol :: [String] -> Int
sol ll = sum (map value pns)
  where
    nn = getNumbers ll
    ss = getSymbols ll
    pns = getPartNumbers nn ss

-- Puz 2 -----------------------------------------------------------------------

test2Input =
  [ "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  ]

test2Output = 467835

data Gear = Gear Symbol Number Number deriving (Eq, Show)

adjacentToSymbol :: Symbol -> [Number] -> [Number]
adjacentToSymbol s = filter (\n -> isAdjacent s n)

getGears :: [Symbol] -> [Number] -> [Gear]
getGears [] _ = []
getGears (s : ss) nn =
  if numAdj == 2
    then Gear s (adjNums !! 0) (adjNums !! 1) : rest
    else rest
  where
    adjNums = adjacentToSymbol s nn
    numAdj = length adjNums
    rest = getGears ss nn

gearRatio :: Gear -> Int
gearRatio (Gear _ (Number _ _ _ v1) (Number _ _ _ v2)) = v1 * v2

sol2 :: [String] -> Int
sol2 ll = sum $ map gearRatio gg
  where
    ss = getSymbols ll
    nn = getNumbers ll
    gg = getGears ss nn

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 03!"
  putStrLn "Which puzzle would you like to solve? (1 or 2):"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day03.txt" ReadMode
  raw <- hGetContents fHandle
  case (read puzNum) :: Int of
    1 -> print $ sol $ lines raw
    2 -> print $ sol2 $ lines raw
    _ -> print "Invalid puzzle"
