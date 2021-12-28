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
