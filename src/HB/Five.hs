module HB.Five where

-- 5.3
-- a-c
-- b-d
-- c-b
-- d-a
-- e-e

-- curried vs. uncurried -------------------------------------------------------
nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + nonsense b

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + nonsense b

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + nonsense b

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + nonsense b

anonTuple :: (Integer, Bool) -> Integer
anonTuple = \(i, b) -> i + nonsense b

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

-- Sectioning ------------------------------------------------------------------
-- Applying elem to only second argument
sec :: Integer -> Bool
sec = (`elem` [1 .. 10])

-- 5.4 Exercises ---------------------------------------------------------------
-- 1: a
-- 2: d
-- 3: b
-- 4: c
-- 5: a
-- 6: e
-- 7: d (but i cheated on this one and I'm not sure why this is the answer)
-- 8: a
-- 9: c

-- 5.5 Parametricity Exercises -------------------------------------------------
-- 1
-- notID :: a -> a
-- notID x = 2 * x - 1.4

-- 2
choose'1 :: a -> a -> a
choose'1 x y = x

choose'2 :: a -> a -> a
choose'2 x y = y

-- 3
snd' :: a -> b -> b
snd' _ y = y

-- 5.6 -------------------------------------------------------------------------
-- 1: [Char] -> [Char]
-- 2: (Fractional a) => a -> a
-- 3: [Char] -> [Char] (WRONG: Wasn't paying attention. Should be (Int -> [Char]))
-- 4: (Num a) => a -> Bool (WRONG: Should be (Int -> Bool))
-- 5: Char -> Bool

-- 5.8 Chapter Exercises -------------------------------------------------------

-- 1: c
-- 2: a
-- 3: b
-- 4: c

-- 1a: (Num a) => a
-- 1b: (Num a) => (a, [Char])
-- 1c: (Integer, [Char])
-- 1d: Bool
-- 1e: Int
-- 1f: Bool
-- 2: (Num a) => a
-- 3: (Num a) => a -> a
-- 4: (Fractional a) => a
-- 5: [Char]

-- 1: Line 2 will fail because bigNum is not a function, so the argument 10 is extraneous.
-- 2: No errors.
-- 3: Line 3 will fail because b is not a function. Proposed change: c = a 10; d = c 200 (d = 210)
-- 4: In a source file, Line 2 would fail first because c is undefined. In GHCi, Line 1 would fail because it comes before the definition of b.

-- 1: Constrained polymorphic, fully polymorphic, concrete, concrete
-- 2: fully polymorphic, concrete, concrete
-- 3: fully polymorphic, constrained polymorphic, concrete
-- 4: full, full, concrete

-- 1: functionH :: [a] -> a
-- 2: functionC :: (Ord a) => a -> a -> Bool
-- 3: functionS :: (a, b) -> b

-- Function Exercises ----------------------------------------------------------
-- 1
i :: a -> a
i x = x

-- 2
c :: a -> b -> a
c x y = x

-- 3: Yes

-- 4
c' :: a -> b -> b
c' x y = y

-- 5
r :: [a] -> [a]
r [] = []
r (x : xs) = r xs ++ [x]

r' :: [a] -> [a]
r' [] = []
r' (x : xs) = if odd then r xs ++ [x] else x : r xs
  where
    odd = 1 == mod (length xs) 2

-- 6
co :: (b -> c) -> (a -> b) -> a -> c
co f1 f2 = f1 . f2

-- 7
a :: (a -> c) -> a -> a
a fac x = x

-- 8
a' :: (a -> b) -> a -> b
a' fab = fab

-- Debugging Exercises ---------------------------------------------------------
-- 1
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if x > y then fstString x else sndString y
  where
    x = "Singin"
    y = "Somewhere"

-- 2: Increase length of x = "Singin", e.g. x = "I'd better bring an umbrella"

-- 3
main :: IO ()
main = do
  print (1 + 2)
  putStrLn "10"
  print (negate (-1 :: Int))
  print ((+) 0 blah)
  where
    blah = negate 1

-- Type-Kwon-Do ----------------------------------------------------------------
-- 1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

-- 2
data A

data B

data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

-- 3
data X

data Y

data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge fxy fywz = fst . fywz . fxy
