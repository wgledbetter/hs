module XML where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (fold)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Monoid (First (First), getFirst)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Xeno.DOM (Content (Element, Text), Node, attributes, contents, name, parse)

--------------------------------------------------------------------------------

-- | Just parsing
xp1 = parse $ T.encodeUtf8 $ T.pack "<foo k='123'><p>hi</p>ok</foo>"

-- DOM processing --------------------------------------------------------------

xTract = xt1 <$> xp1

data Foo
  = Foo
      -- | K
      Int
      -- | P
      P
      -- | Content
      String
  deriving (Eq, Show)

data P = P String deriving (Eq, Show)

xt1 :: Node -> Maybe Foo
xt1 n = Foo <$> mKInt <*> mP <*> mC
  where
    attrMap = M.fromList $ attributes n
    mK = M.lookup (BS8.pack "k") attrMap
    mKInt = fmap (read . BS8.unpack) mK :: Maybe Int

    -- Get a P from contents
    ncs = nodeContents n
    mPs = map xt2 ncs
    mP = getFirst $ foldMap First mPs -- Choose first matching P

    -- Get a string from contents
    tcs = textContents n
    mC = BS8.unpack <$> listToMaybe tcs

xt2 :: Node -> Maybe P
xt2 n =
  if name n == BS8.pack "p"
    then P <$> mStr
    else Nothing
  where
    mStr = BS8.unpack <$> listToMaybe (textContents n)

nodeContents :: Node -> [Node]
nodeContents = map extractNode . filter isNode . contents
  where
    isNode c = case c of
      Element _ -> True
      _ -> False
    extractNode (Element n) = n

textContents :: Node -> [BS.ByteString]
textContents = map extractText . filter isText . contents
  where
    isText c = case c of
      Text _ -> True
      _ -> False
    extractText (Text t) = t
