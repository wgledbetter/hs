--------------------------------------------------------------------------------
----------------------------- Ch. 2: Starting Out ------------------------------
--------------------------------------------------------------------------------
5 /= 5 -- neq

max 0 5 -- Two args only

succ 9 + max 5 4 + 1 == (succ 9) + (max 5 4) + 1 -- Function precedence
succ 9 * 10 /= succ (9 * 10)

div 5 4 == 5 `div` 4 -- infix with `

-- Defining Functions ----------------------------------------------------------
doubleMe x = x + x

doubleMe 8

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

-- Lists -----------------------------------------------------------------------
[1, 2, 3, 4] ++ [9, 10, 11, 12]
'A' : " SMALL CAT"
5 : [1, 2, 3, 4, 5]

[9.4, 33.2, 96.2, 11.2, 23.25] !! 1

[3, 2, 1] > [2, 1, 0] -- True
[3, 2, 1] > [2, 10, 100] -- True
[3, 4, 2] > [3, 4] -- True
[3, 4, 2] > [2, 4] -- True
[3, 4, 2] == [3, 4, 2] -- True

-- head, tail, last, init, length, null, reverse, take, drop, minimum, maximum, sum, product, elem

[1 .. 20]
['a' .. 'z']
[2, 4 .. 20]
[3, 6 .. 20]
[20, 19 .. 1]

-- cycle, repeat, replicate

[x * 2 | x <- [1 .. 10]]
[x * 2 | x <- [1 .. 10], x * 2 >= 12]
[x | x <- [50 .. 100], x `mod` 7 == 3]

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

boomBangs [7 .. 13]

[x | x <- [10 .. 20], x /= 13, x /= 15, x /= 19]
[x * y | x <- [2, 5, 10], y <- [8, 10, 11]]
[x * y | x <- [2, 5, 10], y <- [8, 10, 11], x * y > 50]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

let xxs = [[1, 3, 5, 2, 3, 1, 2, 4, 5], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 4, 2, 1, 6, 3, 1, 3, 2, 3, 6]] in [[x | x <- xs, even x] | xs <- xxs]

-- Tuples ----------------------------------------------------------------------
fst (8, 11)
snd ("Wow", False)

zip [1, 2, 3, 4, 5] [5, 5, 5, 5, 5]
zip [5, 3, 2, 6, 2, 7, 2, 5, 4, 6, 6] ["im", "a", "turtle"]
zip [1 ..] ["apple", "orange", "cherry", "mango"]

triangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. 10], a <- [1 .. 10]]

rightTriangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2]

rightTriangles' = [(a, b, c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2, a + b + c == 24]

--------------------------------------------------------------------------------
------------------------- Ch. 3: Types and Typeclasses -------------------------
--------------------------------------------------------------------------------
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Int, Integer, Float, Double, Bool, Char

-- Type Variables --------------------------------------------------------------
head' :: [a] -> a
fst' :: (a, b) -> a
-- Typeclasses 101 -------------------------------------------------------------
-- Eq, Ord, Show, Read, Enum, Bounded, Num, Integral, Floating

