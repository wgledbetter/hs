-- More Functional Patterns

module HB.Ch7 where

lam = (\x -> x * 5)

-- Anonymous Exercises ---------------------------------------------------------

-- 1: A and D
p227a x y z = x * y * z

p227d = \x -> \y -> \z -> x * y * z

-- 2: D

-- 3a:
addOneIfOdd = \n -> case odd n of
  True -> (\x -> x + 1) n
  False -> n

-- 3b:
addFive = \x -> \y -> (\z -> z + 5) $ if x > y then y else x

-- 3c:
mflip f x y = f y x

-- 7.4 Pattern Matching --------------------------------------------------------

-- The `newtype` keyword only permits a single constructor with a single field.
newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistered User"
printUser
  ( RegisteredUser
      (Username name)
      (AccountNumber accountNumber)
    ) = putStrLn $ name ++ " " ++ show accountNumber

data WherePenguinsLive
  = Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

whereHeIs :: Penguin -> WherePenguinsLive
whereHeIs (Peng place) = place

pengIsFromGalapagos :: Penguin -> Bool
pengIsFromGalapagos (Peng Galapagos) = True
pengIsFromGalapagos _ = False

pengIsFromAntarctica :: Penguin -> Bool
pengIsFromAntarctica (Peng Antarctica) = True
pengIsFromAntarctica _ = False

antOrGalap :: Penguin -> Bool
antOrGalap p = (pengIsFromGalapagos p) || (pengIsFromAntarctica p)

-- Pattern Matching Exercises --------------------------------------------------

-- 1a: (a,b) -> a
-- 1b: String. No, it is a different type than k1 or k3
-- 1c: Both k1 and k3

-- 2
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

-- 7.5: Case Expressions -------------------------------------------------------

funcZ x = case x + 1 of
  1 -> "Wow that's great."
  _ -> "But why"

-- Case Exercises --------------------------------------------------------------

-- 1:
functionC x y = case x > y of
  True -> x
  False -> y

-- 2:
ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

-- 3:
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

-- 7.6: Higher Order Functions -------------------------------------------------

-- returnBroke :: (((a -> b) -> c) -> d) -> d

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

isBossOf :: Employee -> Employee -> Bool
e1 `isBossOf` e2 = e1 > e2

reportBoss :: Employee -> Employee -> IO ()
reportBoss e1 e2 =
  if e1 `isBossOf` e2
    then putStrLn $ show e1 ++ " is the boss of " ++ show e2
    else putStrLn $ show e2 ++ " is the boss of " ++ show e1

-- High-Order Function Exercises -----------------------------------------------
dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1

oneIsTwo :: (Num a) => a -> a
oneIsTwo = (flip dodgy) 2

-- 1: 1
-- 2: 11
-- 3: 22
-- 4: 21
-- 5: 12
-- 6: 11
-- 7: 21
-- 8: 21
-- 9: 22
-- 10: 31
-- 11: 23

-- 7.7: Guards -----------------------------------------------------------------

myAbs :: Integer -> Integer
myAbs x
  | x < 0 = (-x)
  | otherwise = x

isRightTri :: (Num a, Eq a) => a -> a -> a -> String
isRightTri a b c
  | a ^ 2 + b ^ 2 == c ^ 2 = "Ya"
  | otherwise = "Nah"

-- Guard Exercises -------------------------------------------------------------

-- 1:
badGuardOrder1 :: (Fractional a, Ord a) => a -> Char
badGuardOrder1 x
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  where
    y = x / 100

-- 2:
badGuardOrder2 :: (Fractional a, Ord a) => a -> Char
badGuardOrder2 x
  | y >= 0.9 = 'A'
  | y >= 0.7 = 'C'
  | y >= 0.8 = 'B'
  | otherwise = 'F'
  where
    y = x / 100

-- 3: B
pal xs
  | xs == reverse xs = True
  | otherwise = False

-- 4: (Eq a) => [a]
-- 5: (Eq a) => [a] -> Bool

-- 6: C
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- 7: (Ord a, Num a) => a
-- 8: (Ord a, Num a, Num b) => a -> b

-- 7.8: Function Composition ---------------------------------------------------

-- Omission of $ results in (negate . 15), which is bad.
t1 = negate . sum $ [1, 2, 3, 4, 5]

t2 = (negate . sum) [1, 2, 3, 4, 5]

firstFiveOddFrom = take 5 . filter odd . enumFrom

-- 7.9: Point-free Style -------------------------------------------------------
-- "point-free" refers to argument omission, like I just did with firstFiveOddFrom

countLowercaseA = length . filter (== 'a')

addOne = (+ 1)

t3 = addOne . addOne . addOne . negate . addOne $ 0

-- 7.10: Demonstrating composition ---------------------------------------------

-- myPrint = putStrLn . show

-- 7.11: Exercises -------------------------------------------------------------

-- Set 1:
-- 1: D
-- 2: B
-- 3: D
-- 4: B
-- 5: A

-- Set 2:
-- 1a:
tensDigit :: Integral a => a -> a
tensDigit x = (fst . divMod x) 10

-- 1b: Yes
-- 1c:
hundsDigit :: Integral a => a -> a
hundsDigit = tensDigit . tensDigit

-- 2
foldBool :: a -> a -> Bool -> a
foldBool x y b
  | b = y
  | otherwise = x

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 4
-- 5
roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

-- 6
roundTripTypes :: (Show a, Read b) => a -> b
roundTripTypes = read . show

p6 = (roundTripTypes 1) :: Double

-- Summary: --------------------------------------------------------------------

-- Product types "contain" multiple things. Sum types can "be" multiple things.
-- Product type: data Prod = Prod Int Double Bool
-- Sum type: data Sum = Mode1 | Mode2 Bool | Mode3 Double
-- ... I think. Yes.

data Doer = DoInt (Int -> Int) Int

di = DoInt (\x -> x + 1) 2

okayGoAhead :: Doer -> Int
okayGoAhead (DoInt f x) = f x

finally = okayGoAhead di
