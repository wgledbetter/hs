{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Chapter Exercises

module HB.Ch24.Pt11 where

import Control.Applicative (liftA2, many, some, (<|>))
import Data.Attoparsec.Text
import Data.Bits
import Data.Char
import Data.List (foldr, map, sort)
import Data.Map (empty, insertWith, toList)
import qualified Data.Text as T
import Data.Text.Internal.Read (hexDigitToInt)
import Data.Word
import Text.Parser.Char (alphaNum, newline)
import Text.Parser.Token
import Text.RawString.QQ
import Text.Trifecta (eof)

-- 1: SemVer -------------------------------------------------------------------

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

parseSemVer :: Parser SemVer
parseSemVer = do
  maj <- integer
  char '.'
  min <- integer
  char '.'
  pat <- integer
  rel <- pRel
  met <- pMet
  return (SemVer maj min pat rel met)

pRel :: Parser Release
pRel =
  try
    ( do
        char '-'
        pNOSList
    )
    <|> return []

pMet :: Parser Metadata
pMet =
  try
    ( do
        char '+'
        pNOSList
    )
    <|> return []

pNOS :: Parser NumberOrString
pNOS = try noss <|> nosi
  where
    noss = NOSS <$> some letter
    nosi = NOSI <$> integer

-- Feels like there's probably a simpler way.
pNOSList :: Parser [NumberOrString]
pNOSList = do
  try
    ( do
        first <- pNOS
        rest <- try (char '.' >> pNOSList) <|> return []
        return (first : rest)
    )
    <|> return []

testInputs1 :: [T.Text]
testInputs1 =
  [ T.pack "2.1.1",
    T.pack "1.0.0-x.7.z.92",
    T.pack "1.0.0-gamma+002",
    T.pack "1.0.0-beta+oof.sha.41af286"
  ]

test1 :: IO ()
test1 = do
  let pp = parseOnly parseSemVer
      res = map pp testInputs1

  mapM_ print res

instance Ord SemVer where
  compare (SemVer mj1 mn1 pt1 _ _) (SemVer mj2 mn2 pt2 _ _) = compare (mj1, mn1, pt1) (mj2, mn2, pt2)

-- 2: Positive Integers --------------------------------------------------------

parseDigit :: Parser Char
parseDigit =
  try (char '0')
    <|> try (char '1')
    <|> try (char '2')
    <|> try (char '3')
    <|> try (char '4')
    <|> try (char '5')
    <|> try (char '6')
    <|> try (char '7')
    <|> try (char '8')
    <|> (char '9')

base10Integer :: Parser Integer
base10Integer = out
  where
    charList = some parseDigit
    intList :: Parser [Integer]
    intList = map (toInteger . digitToInt) <$> charList
    out = do
      ints <- intList
      return (sum [i * 10 ^ j | (i, j) <- zip (reverse ints) [0 ..]])

test2 :: IO ()
test2 = do
  let pd s = parseOnly parseDigit (T.pack s)
      pb s = parseOnly base10Integer (T.pack s)

  print $ pd "123"
  putStrLn "-------------------------------------------------------------------"
  print $ pd "abc"
  putStrLn "-------------------------------------------------------------------"
  print $ pb "123abc"
  putStrLn "-------------------------------------------------------------------"
  print $ pb "abc"
  putStrLn "-------------------------------------------------------------------"

-- 3: Also Negative Integers ---------------------------------------------------

base10Integer' :: Parser Integer
base10Integer' = do
  sign <- try (char '-' >> return '-') <|> return '+'
  int <- base10Integer
  let int' = case sign of
        '-' -> -int
        '+' -> int

  return int'

test3 :: IO ()
test3 = do
  let pi s = parseOnly base10Integer' (T.pack s)

  print $ pi "-123abc"

-- 4: Phone Numbers ------------------------------------------------------------

type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

intListToInt :: (Integral a) => [a] -> a
intListToInt ints = sum [i * 10 ^ j | (i, j) <- zip (reverse ints) [0 ..]]

parsePhone :: Parser PhoneNumber
parsePhone = do
  try (char '1' >> char '-') <|> return 'x'
  try (char '(') <|> return 'x'
  npa <- parse3Int
  try (char ')') <|> return 'x'
  try (char ' ') <|> return 'x'
  try (char '-') <|> return 'x'
  ex <- parse3Int
  try (char '-') <|> return 'x'
  ln <- parse4Int
  return (PhoneNumber npa ex ln)

parse3Int :: Parser Int
parse3Int = do
  d1 <- parseDigit
  d2 <- parseDigit
  d3 <- parseDigit
  let int = intListToInt $ map digitToInt [d1, d2, d3]
  return int

parse4Int :: Parser Int
parse4Int = do
  d1 <- parseDigit
  d2 <- parseDigit
  d3 <- parseDigit
  d4 <- parseDigit
  let int = intListToInt $ map digitToInt [d1, d2, d3, d4]
  return int

test4 :: IO ()
test4 = do
  let pp s = parseOnly parsePhone (T.pack s)

  print $ pp "123-456-7890"
  print $ pp "1234567890"
  print $ pp "(123) 456-7890"
  print $ pp "1-123-456-7890"

-- 5: Logs ---------------------------------------------------------------------

data Time = Time Int Int deriving (Eq, Show, Ord)

data Date = Date Int Int Int deriving (Eq, Show, Ord)

data Entry = Entry Date Time String deriving (Eq, Show)

instance Ord Entry where
  compare (Entry d1 t1 _) (Entry d2 t2 _) = compare (d1, t1) (d2, t2)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : []) = []
pairs (x : y : l) = (x, y) : pairs (y : l)

dateDiff :: Date -> Date -> Time
dateDiff (Date y1 m1 d1) (Date y2 m2 d2) =
  Time
    (24 * (d2 - d1) + 720 * (m2 - m1) + 8760 * (y2 - y1)) -- Hours
    0 -- Minutes

timeDiff :: Time -> Time -> Time
timeDiff (Time h1 m1) (Time h2 m2) = Time (hrMod + h2 - h1) (m + hrMod * (-60))
  where
    m = m2 - m1
    hrMod = if m < 0 then -1 else 0

addTime :: Time -> Time -> Time
addTime (Time h1 m1) (Time h2 m2) = Time (h1 + h2 + if hrMod then 1 else 0) (mod m 60)
  where
    m = m1 + m2
    hrMod = m >= 60

entryDuration :: Entry -> Entry -> Time
entryDuration (Entry startDate startTime _) (Entry endDate endTime _) =
  addTime
    (dateDiff startDate endDate)
    (timeDiff startTime endTime)

sumEntryTime :: [Entry] -> [(String, Time)]
sumEntryTime es = toList combinedMap
  where
    oes = sort es
    indivDurs = map (\(e1@(Entry _ _ what), e2) -> (what, entryDuration e1 e2)) (pairs oes)
    combinedMap = foldr (\(k, v) m -> insertWith addTime k v m) empty indivDurs

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

sampleEntries :: [Entry]
sampleEntries =
  [ Entry (Date 2024 07 26) (Time 11 50) "Haskell Book Ch24 Exercises",
    Entry (Date 2024 07 26) (Time 10 00) "Picture frame store",
    Entry (Date 2024 07 26) (Time 08 30) "Breakfast"
  ]

parseEntryWithDate :: Date -> Parser Entry
parseEntryWithDate d = do
  hr <- integer
  char ':'
  mn <- integer
  whiteSpace
  what <-
    some
      ( try (parseComment *> return ' ')
          <|> alphaNum
          <|> char ' '
          <|> char ','
          <|> char ';'
          <|> char '-'
          <|> char '&'
          <|> char '?'
          <|> char '!'
      )
  return (Entry d (Time (fromIntegral hr) (fromIntegral mn)) (trim what))

parseComment :: Parser String
parseComment =
  string (T.pack "--")
    >> some
      ( alphaNum
          <|> char ' '
          <|> char '?'
      )

parseDate :: Parser Date
parseDate = do
  char '#'
  char ' '
  y <- integer
  char '-'
  m <- integer
  char '-'
  d <- integer
  return (Date (fromIntegral y) (fromIntegral m) (fromIntegral d))

parseDayEntries :: Date -> Parser [Entry]
parseDayEntries d = many $ do
  try (parseComment <* newline) <|> return ""
  e <- parseEntryWithDate d
  try newline <|> return 'x'
  return e

sampleEntryLine :: T.Text
sampleEntryLine = T.pack "08:00 Breakfast -- of champions"

sampleMultipleEntryLines :: T.Text
sampleMultipleEntryLines =
  [r|08:00 Breakfast -- gross
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

sampleDay :: T.Text
sampleDay =
  [r|# 2025-02-05 -- Rainy
08:00 Breakfast -- gross
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

sampleLog :: T.Text
sampleLog =
  [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

parseDay :: Parser [Entry]
parseDay = parseDate >>= parseDayEntries

parseLog :: Parser [Entry]
parseLog = do
  days <- some (many (newline <|> parseComment *> newline) *> parseDay <* many newline)
  eof
  return $ concat days

test5 :: IO ()
test5 = do
  let pl t = parseOnly parseLog t
  print (sumEntryTime <$> pl sampleLog)

-- Skipping the quickcheck bonus points even though that'd be really nice to have a handle on.

-- 6: IPv4 ---------------------------------------------------------------------

data IPAddress
  = IPv4 Word32
  | IPv6 Word64 Word64
  deriving (Eq, Ord)

combineIPv4Bits :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
combineIPv4Bits a b c d =
  shift (fromIntegral a) 24
    + shift (fromIntegral b) 16
    + shift (fromIntegral c) 8
    + fromIntegral d

parseWord8 :: Parser Word8
parseWord8 = fromIntegral <$> integer

parseIPv4 :: Parser IPAddress
parseIPv4 = do
  a <- parseWord8
  char '.'
  b <- parseWord8
  char '.'
  c <- parseWord8
  char '.'
  d <- parseWord8
  return $ IPv4 (combineIPv4Bits a b c d)

-- 7: IPv6 ---------------------------------------------------------------------

combineBits :: (Bits b, Bits c, Num c, Integral a, Integral b) => a -> b -> c
combineBits a b = a' + b'
  where
    Just bSize = bitSizeMaybe b -- I just didn't want to deal with downstream maybes
    a' = shift (fromIntegral a) bSize
    b' = fromIntegral b

combineIPv6Bits :: Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> (Word64, Word64)
combineIPv6Bits a b c d e f g h =
  ( combineBits (combineBits a b :: Word32) (combineBits c d :: Word32),
    combineBits (combineBits e f :: Word32) (combineBits g h :: Word32)
  )

readHex :: String -> Int
readHex str = sum [i * 16 ^ j | (i, j) <- zip (reverse $ map hexDigitToInt str) [0 ..]]

parseIPv6Quartet :: Parser Word16
parseIPv6Quartet = do
  q <- some alphaNum
  return (fromIntegral $ readHex q)

splitIPv4Words :: IPAddress -> (Word16, Word16)
splitIPv4Words (IPv4 w32) =
  ( fromIntegral $ shiftR w32 16,
    fromIntegral $ shiftR (rotate w32 16) 16
  )

parseLastQuartets :: Parser [Word16]
parseLastQuartets =
  try -- Try IPv4 mapping (unenforced assumption of the "ffff" quartet)
    ( do
        (a, b) <- splitIPv4Words <$> parseIPv4
        return [a, b]
    )
    <|> ( do
            -- Just get last v6 quartet
            last <- parseIPv6Quartet
            return [last]
        )

parseIPv6 :: Parser IPAddress
parseIPv6 = do
  -- Check if
  fillFront <- try (string "::" *> return True) <|> return False
  leadQuarts <- many (parseIPv6Quartet <* char ':')
  let nLead = length leadQuarts
  allQuarts <- -- A list of quarts that is strongly assumed to be of length 8
    if fillFront
      then do
        lastQuarts <- parseLastQuartets
        let nQuarts = nLead + length lastQuarts
        return $ replicate (8 - nQuarts) 0 ++ leadQuarts ++ lastQuarts
      else do
        fillCenter <- try (char ':' *> return True) <|> return False
        if fillCenter -- May also imply filling to the end
          then do
            midQuarts <- many (parseIPv6Quartet <* char ':')
            lastQuarts <- try parseLastQuartets <|> return []
            let nMidQuarts = length midQuarts
                nLastQuarts = length lastQuarts
                nFill = 8 - nLead - nMidQuarts - nLastQuarts
            return $ leadQuarts ++ (replicate nFill 0) ++ midQuarts ++ lastQuarts
          else do
            lastQuarts <- parseLastQuartets
            return (leadQuarts ++ lastQuarts)

  let [w1, w2, w3, w4, w5, w6, w7, w8] = allQuarts -- Here's that length 8 assumption
      (w32a, w32b) = combineIPv6Bits w1 w2 w3 w4 w5 w6 w7 w8
  return $ IPv6 w32a w32b

-- 8: Show ---------------------------------------------------------------------

instance Show IPAddress where
  -- show (IPv4 e) = (show (shiftR e 12)) ++ "." ++ (show (shiftR e 8)) ++ "." ++ () ++ "." ++ ()
  show (IPv6 a b) = undefined
