-- Chapter 4: Basic Datatypes

module HB.Four where

data Mood = Blah | Woot
  deriving (Show)

-- Type constructor: Mood

-- Match argument on data constructor (value)
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

-- Fractional
--   Float
--   Double
--   Rational
--   Fixed
--   Scientific (I like that this exists. I like it a lot.)

-- Exercises -------------------------------------------------------------------
-- 8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = reverse l == l

-- 9
myAbs :: Integer -> Integer
myAbs i = if i > 0 then i else (- i)

-- 10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

-- 1
f2 :: [a] -> Int
f2 xs = w `x` 1
  where
    x = (+)
    w = length xs

-- 2
ident :: a -> a
ident = (\x -> x)

-- 3
f3 :: (a, b) -> a
f3 = fst

-- 1: c
-- 2: b
-- 3: a
-- 4: d
