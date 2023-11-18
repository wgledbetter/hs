module HB.Ch02 where

import qualified GHC.Float
import Prelude
  ( Bool
      ( False,
        True
      ),
    Eq,
    Fractional,
    Int,
    Num (abs),
    negate,
    not,
    pi,
    ($),
    (*),
    (+),
    (-),
    (/),
    (==),
    (^),
  )

-- Haskell Book ----------------------------------------------------------------

-- 2.5 Exercises ---------------------------------------------------------------
-- 1
half :: (Num a, Fractional a) => a -> a
half = (/ 2)

square :: (Num a) => a -> a
square x = x * x

-- 2
twoFiveTwo :: (Num a, Fractional a) => a -> a
twoFiveTwo x = (*) 3.14 $ square x

-- 3
twoFiveThree :: (Num a, Fractional a, GHC.Float.Floating a) => a -> a
twoFiveThree x = (*) pi $ square x

-- 2.6 Exercises ---------------------------------------------------------------
-- 1
twoSixOne :: Bool
twoSixOne = (8 + 7 * 9) == ((8 + 7) * 9)

twoSixOneAnswer :: Bool
twoSixOneAnswer = Prelude.not twoSixOne

-- 2
twoSixTwo :: (Num a, Eq a) => a -> a -> Bool
twoSixTwo x y = ((x * 2) + (y * 2)) == (x * 2 + y * 2)

twoSixTwoAnswer :: Bool
twoSixTwoAnswer = twoSixTwo 4 76

-- 2.7 Exercises ---------------------------------------------------------------
-- 1
area :: (Num a, Fractional a) => a -> a
area x = 3.14 * (x * x)

-- 2
double :: (Num a, Fractional a) => a -> a
double x = x * 2

-- 3
-- Indentation

-- 2.9: Sectioning -------------------------------------------------------------
section :: (Num a) => a -> a
section = (+ 4)

-- 2.10: let and where ---------------------------------------------------------
-- let: expression
-- where: declaration

twoTenOneLet :: Int
twoTenOneLet = let x = 5 in x

twoTenTwoLet :: Int
twoTenTwoLet = let x = 5 in x * x

twoTenThreeLet :: Int
twoTenThreeLet = let x = 5; y = 6 in x * y

twoTenFourLet :: Int
twoTenFourLet = let x = 3; y = 1000 in x + 3

twoTenOneWhere = x * 3 + y where x = 3; y = 1000

twoTenTwoWhere = x * 5 where y = 10; x = 10 * 5 + y

twoTenThreeWhere :: GHC.Float.Double
twoTenThreeWhere = z / x + y where x = 7; y = negate x; z = y * 10

-- 2.11: Exercises -------------------------------------------------------------
-- Parenthesization
parenOne :: Bool
parenOne = (2 + 2 * 3 - 1) == (2 + (2 * 3) - 1)

parenTwo :: Bool
parenTwo = ((^) 10 $ 1 + 1) == ((^) 10 (1 + 1))

parenThree :: Bool
parenThree = (2 ^ 2 * 4 ^ 5 + 1) == (((2 ^ 2) * (4 ^ 5)) + 1)

-- Expression equivalence
-- 1: True
-- 2: False: 100 /= 910
-- 3: False: subtraction order matters
-- 4: False: integral vs floating division
-- 5: False: parentheses change order of operations

-- REPL Rewrite (define before use, vs source where order doesn't matter)
z :: Int
z = 7

y :: Int
y = z + 8

x :: Int
x = y ^ 2

waxOn :: Int
waxOn = x * 5
  where
    z = 7
    y = z + 8
    x = y ^ 2

-- 5
triple :: (Num a) => a -> a
triple = (* 3)
