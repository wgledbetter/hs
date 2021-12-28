-- Data ------------------------------------------------------------------------
data Minutes = Minutes Int

addMin :: Minutes -> Minutes -> Minutes
addMin (Minutes x) (Minutes y) = Minutes (x + y)

-- Codelab ---------------------------------------------------------------------
-- 01: Functions ---------------------------------------------------------------
add :: Int -> Int -> Int
add x y = x + y

subtract :: Int -> Int -> Int
subtract x y = x - y

double :: Int -> Int
double x = 2 * x

multiply :: Int -> Int -> Int
multiply x y = x * y

divide :: Int -> Int -> Double
divide x y = fromIntegral x / fromIntegral y

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n -1)

gcd' :: Int -> Int -> Int
gcd' a b
  | a < b = gcd' b a
  | remAB > 0 = gcd' b remAB
  | otherwise = b
  where
    remAB = rem a b

-- 02: Datatypes ---------------------------------------------------------------
hours :: Minutes -> Int
hours (Minutes mm) = div mm 60

timeDistance :: Minutes -> Minutes -> Minutes
timeDistance (Minutes mm1) (Minutes mm2)
  | mm1 < mm2 = timeDistance (Minutes mm2) (Minutes mm1)
  | otherwise = Minutes (mm1 - mm2)

type Point = (Int, Int)

pointDistance :: Point -> Point -> Double
pointDistance p1 p2 = sqrt (fromIntegral (x2 - x1)) ^ 2 + fromIntegral (y2 - y1) ^ 2
  where
    x1 = fst p1
    y1 = snd p1
    x2 = fst p2
    y2 = snd p2
