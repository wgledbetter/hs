-- Parser Combinators
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HB.Ch24 where

import Control.Applicative
import Control.Monad.Fail (fail)
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.Text (parseOnly)
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio ((%))
import Data.String (IsString)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.Parsec (Parsec, parseTest)
import Text.RawString.QQ -- Package: raw-strings-qq
import Text.Trifecta
  ( CharParsing,
    Parser,
    Result (Success),
    TokenParsing,
    char,
    decimal,
    double,
    eof,
    integer,
    letter,
    newline,
    noneOf,
    oneOf,
    parseByteString,
    parseString,
    skipMany,
    skipSome,
    string,
    try,
    unexpected,
  )

-- 24.3 ------------------------------------------------------------------------

-- Parser Combinator takes parsers and generates another parser
-- "Combinators are expressions with no free variables"

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

-- Very similar to State, just with a failure case (Maybe)
type MyParser a = String -> Maybe (a, String)

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> String -> IO ()
testParse p s = print $ parseString p mempty s

pNL s = putStrLn ('\n' : s)

ch243main = do
  pNL "stop:"
  testParse stop "123"

  pNL "one:"
  testParse one "123"

  pNL "one':"
  testParse one' "123"

  pNL "oneTwo:"
  testParse oneTwo "123"

  pNL "oneTwo':"
  testParse oneTwo' "123"

-- Exercises

-- 1:

ex1a s = print $ parseString (one >> eof) mempty s

ex1b s = print $ parseString (oneTwo >> eof) mempty s

-- 2:

pStr1 = string "1"

pStr12 = string "12"

pStr123 = string "123"

pStrWOW = string "WOW"

p123 s = out
  where
    st = parseString stop mempty s
    ps = [pStr123, pStr12, pStr1, pStrWOW] -- Order matters b/c they don't use eof b/c I can't get EOF to return the result I care about.
    ii = map (\p -> parseString p mempty s) ps -- Attempt all parsers
    out = foldr (<|>) st ii -- Use alternative to pull first success

-- 3:

-- Gives whole string on success, but failures only show next expected char.
myStrParser :: (CharParsing m) => String -> m String
myStrParser = traverse char

-- 24.4: Parsing Fractions -----------------------------------------------------

badFrac = "1/0"

alsoBad = "10"

good = "1/2"

gooder = "2/1"

parseFrac :: Parser Rational
parseFrac = do
  numer <- decimal
  char '/'
  denom <- decimal
  case denom of
    0 -> fail "divide by zero" -- not an exception/bottom
    _ -> return (numer % denom)

pf s = parseString parseFrac mempty s

-- Exercise

-- This function solves my problem up on l63 with eof returning ()
getFromEOF p = do
  parsed <- p
  eof
  return parsed

-- 24.5: Haskell's Parsing Ecosystem -------------------------------------------

-- "rewinds" parser on failure
-- try:: m a -> m a

-- 24.6: Alternative -----------------------------------------------------------

type NumberOrString = Either Integer String

a = "blah"

b = "123"

c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

ch246main :: IO ()
ch246main = do
  let p f i = parseString f mempty i

  print $ p (some letter) a
  print $ p integer b

  print $ p parseNos a
  print $ p parseNos b

  print $ p (many parseNos) c
  print $ p (some parseNos) c

  print $ p (many parseNos) "" -- Succeeds because "many" is "zero or more"
  print $ p (some parseNos) "" -- Fails because "some" is "one or more"

-- QuasiQuotes

eitherOr :: String
eitherOr =
  [r|
123
abc
456
def
|]

betterParseNos :: Parser NumberOrString
betterParseNos = do
  skipMany (oneOf "\n")
  v <- parseNos
  skipMany (oneOf "\n")
  return v

pqq = parseString (many betterParseNos) mempty eitherOr

-- Could also wrap parseNos in "token", which strips trailing whitespace.

-- Exercise: try try

type FracOrDub = Either Rational Double

parseDouble :: Parser Double
parseDouble = try double <|> (fromIntegral <$> decimal)

parseFracOrDub :: Parser FracOrDub
parseFracOrDub = (try $ Left <$> parseFrac) <|> (Right <$> parseDouble)

-- 24.7: Parsing Config Files --------------------------------------------------

headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

phTest = parseByteString parseHeader mempty headerEx

assignmentEx :: ByteString
assignmentEx = "nm=val"

newtype Name = Name String deriving (Eq, Ord, Show)

newtype Value = Value String deriving (Eq, Ord, Show)

type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- Name <$> some letter
  _ <- char '='
  value <- Value <$> some (noneOf "\n")
  _ <- skipEOL
  return (name, value)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

paTest = parseByteString parseAssignment mempty assignmentEx

commentEx :: ByteString
commentEx = "; omg im sooooo sorry about this code uwu"

commentEx' :: ByteString
commentEx' = "; blah\n; wow\n  \n;ass"

skipComments :: Parser ()
skipComments =
  skipMany
    ( do
        _ <- char ';' <|> char '#'
        skipMany (noneOf "\n")
        skipEOL
    )

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' =
  [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' =
  [r|
; comment
[section]
host=wikipedia.org
alias=claw
[whatisit]
red=intoothandclaw
|]

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments

  h <- parseHeader
  skipEOL

  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

psTest1 = parseByteString parseSection mempty sectionEx

psTest2 = parseByteString parseSection mempty sectionEx'

psTest3 = parseByteString parseSection mempty sectionEx''

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)

maybeParsed :: Result a -> Maybe a
maybeParsed (Success x) = Just x
maybeParsed _ = Nothing

testIniParser :: IO ()
testIniParser = hspec $ do
  -- Assignment
  describe "Assignment Packing" $ it "can parse a simple assignment" $ do
    let m = parseByteString parseAssignment mempty assignmentEx
        r' = maybeParsed m
    print m
    r' `shouldBe` Just (Name "nm", Value "val")

  -- Headers
  describe "Header Parsing" $ it "can parse a simple header" $ do
    let m = parseByteString parseHeader mempty headerEx
        r' = maybeParsed m
    print m
    r' `shouldBe` Just (Header "blah")

  -- Comments
  describe "Comment parsing" $ it "Skips comment before header" $ do
    let p = skipComments >> parseHeader
        i = "; woot\n[blah]"
        m = parseByteString p mempty i
        r' = maybeParsed m
    print m
    r' `shouldBe` Just (Header "blah")

  -- Sections
  describe "Section parsing" $ it "can parse a simple section" $ do
    let m = parseByteString parseSection mempty sectionEx
        r' = maybeParsed m
        states = M.fromList [(Name "Chris", Value "Texas")]
        expected' = Just (Section (Header "states") states)
    print m
    r' `shouldBe` expected'

  -- Config
  describe "INI parsing" $ it "Can parse multiple sections" $ do
    let m = parseByteString parseIni mempty sectionEx''
        r' = maybeParsed m
        sectionValues = M.fromList [(Name "alias", Value "claw"), (Name "host", Value "wikipedia.org")]
        whatisitValues = M.fromList [(Name "red", Value "intoothandclaw")]
        expected' =
          Just
            ( Config
                ( M.fromList
                    [ (Header "section", sectionValues),
                      (Header "whatisit", whatisitValues)
                    ]
                )
            )
    print m
    r' `shouldBe` expected'

-- That's cool

-- 24.8: Character and Token Parsers -------------------------------------------

assTest1 = "var=123"

assTest2 = "var=otherVar"

data Ass = AssExpr String String | AssInt String Integer deriving (Eq, Show)

assParser :: Parser Ass
assParser = try assExpr <|> assInt
  where
    assExpr = do
      lhs <- some letter
      _ <- char '='
      exp <- some letter
      return $ AssExpr lhs exp
    assInt = do
      lhs <- some letter
      _ <- char '='
      num <- integer
      return $ AssInt lhs num

-- 24.9: Polymorphic Parsers ---------------------------------------------------

-- Good technique during parser development can allow use of multiple frameworks

badFraction :: (IsString s) => s
badFraction = "1/0"

badderFraction :: (IsString s) => s
badderFraction = "10"

goodFraction :: (IsString s) => s
goodFraction = "1/2"

bestestFraction :: (IsString s) => s
bestestFraction = "2/1"

fractionParser :: (Monad m, TokenParsing m, MonadFail m) => m Rational
fractionParser = do
  numer <- decimal
  _ <- char '/'
  denom <- decimal
  case denom of
    0 -> fail "I think it's weird to catch div-by-zero at parse-time, but whatever."
    _ -> return (numer % denom)

soManyFrameworks :: IO ()
soManyFrameworks = do
  let attoP = parseOnly fractionParser

  print $ attoP badFraction
  print $ attoP badderFraction
  print $ attoP goodFraction
  print $ attoP bestestFraction

  let trf = parseString fractionParser mempty

  print $ trf badFraction
  print $ trf badderFraction
  print $ trf goodFraction
  print $ trf bestestFraction

-- Failure and Backtracking

trifP :: (Show a) => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP :: (Show a) => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: (Show a) => A.Parser a -> ByteString -> IO ()
attoP p i = print $ A.parseOnly p i

nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

failBT :: IO ()
failBT = do
  trifP nobackParse "13"
  putStrLn "-------------------------------------------------------------------"
  trifP tryParse "13"
  putStrLn "-------------------------------------------------------------------"
  parsecP nobackParse "13"
  putStrLn "-------------------------------------------------------------------"
  parsecP tryParse "13"
  putStrLn "-------------------------------------------------------------------"
  attoP nobackParse "13"
  putStrLn "-------------------------------------------------------------------"
  attoP tryParse "13"
  putStrLn "-------------------------------------------------------------------"
