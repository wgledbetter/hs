{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HB.Ch24.Pt10 where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ

-- Input -----------------------------------------------------------------------

sectionJson :: ByteString
sectionJson =
  [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

-- Types -----------------------------------------------------------------------

data TestData = TestData {section :: Host, what :: Color} deriving (Eq, Show)

newtype Host = Host String deriving (Eq, Show)

type Annotation = String

data Color
  = Red Annotation
  | Blue Annotation
  | Yellow Annotation
  deriving (Eq, Show)

-- Typeclass Instances ---------------------------------------------------------

instance FromJSON TestData where
  parseJSON (Object v) = TestData <$> v .: "section" <*> v .: "whatisit"
  parseJSON _ = fail "Expected object"

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _ = fail "Expected object"

instance FromJSON Color where
  parseJSON (Object v) = (Red <$> v .: "red") <|> (Blue <$> v .: "blue") <|> (Yellow <$> v .: "yellow")
  parseJSON _ = fail "Expected object"

-- Main ------------------------------------------------------------------------

ch24_10 :: IO ()
ch24_10 = do
  let d :: Maybe TestData
      d = decode sectionJson
  print d
