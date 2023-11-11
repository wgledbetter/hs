module FPPG.Ch2 where

import HB.Ch2 (x)

-- Composition
square :: (Num a) => a -> a
square = (^ 2)

comp1 :: (Floating a) => a -> a
comp1 x = square (cos x)

comp2 :: (Floating a) => a -> a
comp2 x = square $ cos x

comp3 :: (Floating a) => a -> a
comp3 = square . cos -- point-free style

-- Exercises -------------------------------------------------------------------
-- 1
one :: (Floating a) => a -> a
one = sqrt . (+) 1

-- 2
two :: (Fractional a) => a -> a
two t = -9.81 * t ^ 2 + 30 * t

-- 3
three :: (Fractional a) => a -> a
three t = -9.81 * t + 30

-- 4
sinDeg :: (Floating a) => a -> a
sinDeg = sin . (*) (pi / 180)

-- 5
fiveA :: (Floating a) => a -> a
fiveA x = x ** (1 / 3)

fiveB :: (Floating a) => a -> a
fiveB y = exp y + 8 ** y

fiveC :: (Floating a) => a -> a
fiveC x = recip $ sqrt $ (x - 5.0) ** 2 + 16.0

fiveD :: (Floating a) => a -> a
fiveD b = recip . sqrt $ 1 - b ** 2

fiveE :: (Floating a) => a -> a
fiveE x = recip (10 + x) + recip (10 - x)

fiveF :: (Floating a) => a -> a
fiveF l = sqrt $ l * (l + 1)

fiveG :: (Floating a) => a -> a
fiveG = recip . fiveA . abs

fiveH :: (Floating a) => a -> a
fiveH z = recip $ (z ** 2 + 4) ** (3 / 2)

-- 6
sixA :: (Floating a) => a -> a
sixA = \b -> recip . sqrt $ 1 - b ** 2

sixB :: (Floating a) => a
sixB = sixA 0.8
