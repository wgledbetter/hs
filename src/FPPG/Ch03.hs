module FPPG.Ch03 where

-- 1
oneA :: Bool
oneA = (False || True && False || True) == ((False || (True && False)) || True)

oneB :: Bool
oneB = (2 / 3 / 4 == 4 / 3 / 2) == (((2 / 3) / 4) == ((4 / 3) / 2))

oneC :: Bool
oneC = (7 - 5 / 4 > 6 || 2 ^ 5 - 1 == 31) == ((7 - (5 / 4)) > 6 || (((2 ^ 5) - 1) == 31))

oneD :: Bool
oneD = False

oneE :: Bool
oneE = (2 < 3 && 3 < 4) == ((2 < 3) && (3 < 4))

oneF :: Bool
oneF = False

-- 2
f :: (Num a, Ord a) => a -> a
f x
  | x <= 0 = 0
  | otherwise = x

e :: (Ord a, Floating a) => a -> a
e r
  | r <= 1 = r
  | otherwise = recip $ r ** 2

-- 3
isXorY :: Char -> Bool
isXorY 'X' = True
isXorY 'Y' = True
isXorY _ = False

-- 5
greaterThan50 :: Integer -> Bool
greaterThan50 x
  | x > 50 = True
  | otherwise = False

-- 6
amazingCurve :: Int -> Int
amazingCurve g
  | 0 < g && g < 50 = 2 * g
  | 50 <= g = 100
  | otherwise = 0

-- 9
nineOne :: Bool -> Bool
nineOne x = x

nineTwo :: Bool -> Bool
nineTwo = not

nineThree :: Bool -> Bool
nineThree _ = True

nineFour :: Bool -> Bool
nineFour _ = False

nineFive :: Bool -> Bool -> Bool
nineFive a b = a

nineSix :: Bool -> Bool -> Bool
nineSix a b = b

nineSeven :: Bool -> Bool -> Bool
nineSeven = (&&)

nineEight :: Bool -> Bool -> Bool
nineEight = (||)

nineNine :: Bool -> Bool -> Bool
nineNine a b = not a

nineTen :: Bool -> Bool -> Bool
nineTen a = not

nineEleven :: Bool -> Bool -> Bool
nineEleven a b = True

nineTwelve :: Bool -> Bool -> Bool
nineTwelve a b = False

-- 10
xpr :: Bool
xpr = True || False && False
