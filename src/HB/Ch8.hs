-- Recursion

module HB.Ch8 where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

applyTimes :: (Integral a) => a -> (b -> b) -> b -> b
applyTimes 0 f x = x
applyTimes n f x = f . applyTimes (n - 1) f $ x

applyTimesEx = (+ 1) ((+ 1) ((+ 1) ((+ 1) ((+ 1) (5)))))

-- 8.3: Bottom -----------------------------------------------------------------

data Maybe a = Nothing | Just a

-- We avoid "hitting bottom" from tho omission of a True case by changing the return type.
sortaPartial :: Bool -> HB.Ch8.Maybe Int
sortaPartial False = HB.Ch8.Just 0
sortaPartial _ = HB.Ch8.Nothing

-- 8.4: Fibonacci --------------------------------------------------------------

fibonacci :: (Integral a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)

-- 8.5: Integral Division ------------------------------------------------------

recDiv :: (Integral a) => a -> a -> a
recDiv _ 0 = error "Divide by zero"
recDiv numer denom
  | numer >= denom = 1 + recDiv (numer - denom) denom
  | otherwise = 0

recDivMod :: (Integral a) => a -> a -> (a, a)
recDivMod _ 0 = error "Divide by zero"
recDivMod numer denom
  | numer >= denom = let (d, m) = recDivMod (numer - denom) denom in (1 + d, m)
  | otherwise = (0, numer)

-- 8.6: Exercises --------------------------------------------------------------

-- Set 1: Reviewing Types
-- 1: D
-- 2: B
-- 3: D
-- 4: B

-- Set 2: Reviewing Currying
-- 1: "woops mrow woohoo!"
-- 2: "1 mrow haha"
-- 3: "woops mrow 2 mrow haha"
-- 4: "woops mrow blue mrow haha"
-- 5: "pink mrow haha mrow green mrow woops mrow blue"
-- 6: "are mrow Pugs mrow awesome"

-- Set 3: Recursion
-- 1:
-- recDivMod 15 2 = let (d,m) = recDivMod 13 2 in (1+d, m)
-- recDivMod 13 2 = let (d,m) = recDivMod 11 2 in (1+d, m)
-- recDivMod 11 2 = let (d,m) = recDivMod  9 2 in (1+d, m)
-- recDivMod  9 2 = let (d,m) = recDivMod  7 2 in (1+d, m)
-- recDivMod  7 2 = let (d,m) = recDivMod  5 2 in (1+d, m)
-- recDivMod  5 2 = let (d,m) = recDivMod  3 2 in (1+d, m)
-- recDivMod  3 2 = let (d,m) = recDivMod  1 2 in (1+d, m)
-- recDivMod  1 2 = (0, 1)
-- Thus,
-- recDivMod 15 2 = (1+1+1+1+1+1+1+0, 1) = (7, 1)

-- 2: I broke the rule by adding Ord a to the type signature, but it's either that or a cast to Integral.
recSum :: (Eq a, Num a, Ord a) => a -> a
recSum n
  | n > 1 = n + recSum (n - 1)
  | otherwise = n

-- 3: Positive numbers only
recMult :: (Integral a) => a -> a -> a
recMult a b
  | b == 0 = 0
  | otherwise = a + recMult a (b - 1)

-- Fixing recDiv:
data DivResult a = Ans a | DivByZero deriving (Show)

recDivModSafe :: (Integral a) => a -> a -> (DivResult a, a)
recDivModSafe _ 0 = (DivByZero, 0)
recDivModSafe 0 _ = (Ans 0, 0)
recDivModSafe numer denom
  | (numer < 0) && (denom > 0) = let (d, m) = recDivMod (-numer + denom) denom in (Ans (-d), denom - m)
  | (numer > 0) && (denom < 0) = let (d, m) = recDivMod (numer - denom) (-denom) in (Ans (-d), denom + m)
  | (numer > 0) && (denom > 0) = let (d, m) = recDivMod numer denom in (Ans d, m)
  | (numer < 0) && (denom < 0) = let (d, m) = recDivMod (-numer) (-denom) in (Ans d, -m)

-- McCarthy 91:
mc91 :: (Integral a) => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ x + 11

-- x = 98:
-- mc91(98) = mc91(mc91(109))
--          = mc91(99)
--          = mc91(mc91(110))
--          = mc91(100)
--          = mc91(mc91(111))
--          = mc91(101)
--          = 91

-- Numbers to words
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "TOO BIG"

digits :: Int -> [Int]
digits 0 = []
digits n = let (Ans n', t) = recDivModSafe n 10 in (++) (digits n') [t]

intersperse :: a -> [a] -> [a]
intersperse i (front : rest) = [front] ++ internal rest
  where
    internal (f : r)
      | null ([f] ++ r) = []
      | otherwise = [i] ++ [f] ++ internal r
    internal r = []

wordNumber :: Int -> String
-- wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))
wordNumber = concat . (intersperse "-") . (map digitToWord) . digits
