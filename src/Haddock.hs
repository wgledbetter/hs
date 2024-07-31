{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- https://kowainik.github.io/posts/haddock-tips

-- |
-- Module: wow
-- Description: I am doing the testing of the tool to generate the doxumenz.
-- Copyright: nah
-- License: hazardous
-- Maintainer: wgl
-- Stability: experimental
-- Portability: GHC
--
-- You won't read it anyway.
module Haddock
-- This is where you can organize the page for this module
  ( -- * Basics
    preComment,
    postComment,
    multiLine,
    postMulti,
    multiBlock,
    skipped,
    notSkipped,

    -- * Types and Typeclasses
    -- $putItOverHere

    -- ** subsection
    CharWithInt (..),

    -- ** more difrit #ananchor#
    Eject,
  )
where

-- $putItOverHere
-- Out-of-line words
-- only works in export list

-- Basics ----------------------------------------------------------------------

-- | This is a haddock pre-comment
--
-- ==== __EXPAND ME__
--
-- oh hi
-- here is how you do.
--
-- >>> preComment 12
-- 13
preComment :: Int -> Int
preComment a = a + 1

postComment :: Double -> Double
-- ^ This is a haddock post-comment, I think.
postComment = (-) 1
-- ^ This shows up first...

-- | If I want to
-- do multiline
--
-- You can refer to "HB.Ch12" for some stuff.
multiLine :: Float -> Float
multiLine = (*) 2

postMulti :: Rational -> Int
-- ^ how bout
-- I do like this
postMulti = floor

-- | multiline
-- with
-- blocks
-- Oh, ormolu reformats that.
--
-- You should look at 'CharWithInt'
multiBlock :: IO ()
multiBlock = putStrLn "ass"

-- | this should be
-- with skipped

-- but not this
skipped :: IO ()
skipped = undefined

-- | Here I
-- can do
--
--  newlines
-- But ormolu strikes again.
--
-- check it out [this doesn't seem to be working for me](#ananchor)
notSkipped :: IO ()
notSkipped = undefined

-- Types and Typeclasses -------------------------------------------------------

-- | This type does some stuff I want
data CharWithInt
  = -- | Use post-comment with line-trailers
    Empty
  | -- | Pre comment above constructor, but I don't love this....
    CharCode
      -- | The number I really care about
      Int
      -- | I don't even remember.
      Char
  deriving
    ( -- | so I can compare
      Eq,
      -- | debugging
      Show
    )

-- | for something ejectable
class Eject a where
  -- | got'em
  getInt :: a -> Maybe Int

  -- | gitted
  getChar :: a -> Maybe Char

-- | what makes my data ejectable
-- wait, can I not document instance functions?
instance Eject CharWithInt where
  -- \| By subtracting where it is from where it isn't the missle obtains the variation.
  getInt Empty = Nothing
  -- \| Alternatively, the missile may just follow you home
  getInt (CharCode i _) = Just i

  -- \| I git char
  getChar Empty = Nothing
  -- \| wowo
  getChar (CharCode _ c) = Just c
