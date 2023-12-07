module Day07 where

import Data.List (sort, sortBy)
import HB.Ch09 (splitAndDrop)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInput =
  [ "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  ]

testOutput = 6440

data Card
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Show, Ord, Enum)

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Show, Ord, Enum)

data Hand = Hand Card Card Card Card Card deriving (Eq, Show)

countCards :: Hand -> [(Int, Card)]
countCards (Hand c1 c2 c3 c4 c5) =
  filter (\(n, c) -> n > 0)
    $ foldr
      (\q l -> (length $ filter (\c -> c == q) handList, q) : l)
      []
    $ enumFrom Two
  where
    handList = [c1, c2, c3, c4, c5]

handType :: Hand -> HandType
handType h = case sortedCounts of
  [5] -> FiveOfAKind
  [4, 1] -> FourOfAKind
  [3, 2] -> FullHouse
  [3, 1, 1] -> ThreeOfAKind
  [2, 2, 1] -> TwoPair
  [2, 1, 1, 1] -> OnePair
  _ -> HighCard
  where
    sortedCounts = reverse $ sort $ map fst $ countCards h

instance Ord Hand where
  compare h1@(Hand c11 c12 c13 c14 c15) h2@(Hand c21 c22 c23 c24 c25) =
    if handcomp == EQ
      then
        if c1c == EQ
          then
            if c2c == EQ
              then
                if c3c == EQ
                  then
                    if c4c == EQ
                      then c5c
                      else c4c
                  else c3c
              else c2c
          else c1c
      else handcomp
    where
      handcomp = compare (handType h1) (handType h2)
      c1c = compare c11 c21
      c2c = compare c12 c22
      c3c = compare c13 c23
      c4c = compare c14 c24
      c5c = compare c15 c25

parseCard :: Char -> Card
parseCard '2' = Two
parseCard '3' = Three
parseCard '4' = Four
parseCard '5' = Five
parseCard '6' = Six
parseCard '7' = Seven
parseCard '8' = Eight
parseCard '9' = Nine
parseCard 'T' = Ten
parseCard 'J' = Jack
parseCard 'Q' = Queen
parseCard 'K' = King
parseCard 'A' = Ace

parseHand :: String -> Hand
parseHand (c1 : c2 : c3 : c4 : c5 : []) =
  Hand
    (parseCard c1)
    (parseCard c2)
    (parseCard c3)
    (parseCard c4)
    (parseCard c5)

newtype Bet = Bet Int deriving (Eq, Show, Ord)

parseInputLine :: String -> (Hand, Bet)
parseInputLine il = (parseHand $ head split, Bet (read $ last split))
  where
    split = splitAndDrop ' ' il

parseInput :: [String] -> [(Hand, Bet)]
parseInput = map parseInputLine

sol :: [String] -> Int
sol s = sum winnings
  where
    handsBets = parseInput s
    sortedHandsBets = sortBy (\(h1, _) (h2, _) -> compare h1 h2) handsBets
    winnings = map (\(i, (_, (Bet b))) -> i * b) $ zip [1 ..] sortedHandsBets

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 07!"
  putStrLn "Which puzzle would you like to solve (1)?"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day07.txt" ReadMode
  raw <- hGetContents fHandle
  case (read puzNum) :: Int of
    1 -> (print . sol . lines) raw
    _ -> print "Invalid puzzle"
