-- Type Classes

module HB.Ch06 where

import Data.List (sort)

-- The pattern where you derive from a typeclass which defines a set of
-- functions but you only have to overload/define a subset of all funtions feels
-- extremely similar to the CRTP base class pattern where two functions can
-- depend on each other in the base class and can call the opposite
-- implementation in the derived class. Like in the ASSET architecture how only
-- one of jacobian() and compute_jacobian() needs to be overloaded, but you get
-- the full functionality of both.

-- 6.5 Exercises: Eq -----------------------------------------------------------
-- 1
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn i1) (TisAn i2) = i1 == i2

-- 2
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  Two i11 i12 == Two i21 i22 = i11 == i21 && i12 == i22

-- 3
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  TisAnInt i1 == TisAnInt i2 = i1 == i2
  TisAString s1 == TisAString s2 = s1 == s2
  _ == _ = False

-- 4
data Pair a = Pair a a

instance (Eq a) => Eq (Pair a) where
  Pair x11 x12 == Pair x21 x22 = x11 == x21 && x12 == x22

-- 5
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a1 b1 == Tuple a2 b2 = a1 == a2 && b1 == b2

-- 6
data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
  ThisOne a1 == ThisOne a2 = a1 == a2
  ThatOne a1 == ThatOne a2 = a1 == a2
  ThisOne _ == ThatOne _ = False
  ThatOne _ == ThisOne _ = False

-- 7
data EitherOr a b = A a | B b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  A a1 == A a2 = a1 == a2
  B b1 == B b2 = b1 == b2
  _ == _ = False

-- 6.8 Ord ---------------------------------------------------------------------

-- First constructors are "less than" following constructors
data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq)

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = GT
  compare _ _ = EQ

-- Exercises

-- 1: Valid. Comparing Int with Int
ch68_1 = max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])

-- 2: Valid
ch68_2 = compare (3 * 4) (3 * 5)

-- 3: Invalid
-- ch68_3 = compare "Julie" True

-- 4: Valid
ch68_4 = (5 + 3) > (3 + 6)

-- 6.9 Enum --------------------------------------------------------------------

test69 = enumFromThenTo 0 3 27

-- 6.10 Show -------------------------------------------------------------------

data Mood = Blah | FuckYeah

instance Show Mood where
  show Blah = "Blah"
  show _ = "yeah im okay i guess"

-- 6.11 Read -------------------------------------------------------------------

-- just don't

-- 6.12 Dispatch ---------------------------------------------------------------

-- The type of an argument is used to select the instance implementation.

-- 6.14 ------------------------------------------------------------------------
-- Set 1:
-- 1: C
-- 2: B
-- 3: A
-- 4: C
-- 5: A

-- Set 2:
-- 1: No. The Person type does not implement the Show type class.
data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2: No. Mood does not implement Eq.
instance Eq Mood where
  (==) Blah FuckYeah = False
  (==) _ _ = True

settleDown x = if x == FuckYeah then Blah else x

-- 3a: Mood type is only acceptable input.
-- 3b: Type error because of inferred function type signature
-- 3c: Error. Ord has not been "instanced" for Mood

-- 4: No. Sentence s1 does not provide all constructor arguments.
-- Wrong. s1 just becomes curried, waiting for the last constructor argument.
type Subject = String

type Verb = String

type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"

-- Set 3:
data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- 1: No. The Papu constructor arguments must be Rocks and Yeah, not String and Bool, even though Rocks and Yeah can only be constructed by String and Bool, respectively.
-- 2: Yes. The proper Rocks and Yeah constructors are used.
-- 3: Yes. We derived Eq for all the above types.
-- 4: No. We have not implemented Ord for any of these types.

-- Set 4:
-- 1: No. The assignment of a literal number requires the Num type class.
i :: Num a => a
i = 1

-- 2: No. I looked ahead and I think you need Fractional. Yep.
f :: Float
f = 1.0

-- 3: Yes.
-- 4: Yes. RealFrac requires Fractional
f' :: RealFrac a => a
f' = 1.0

-- 5: Yes. Adding the Ord type class is probably unnecessary, but is valid.
freud :: Ord a => a -> a
freud a = a

-- 6: Yes. Same reasoning. Restricting the type to Int seems unnecessary, but is totally valid.
-- 7: I think so. I think that having the fixed `myX` value will resolve the generic a -> a declaration.
-- Wrong. The type is saying "this must be valid for all unrestricted a," and we've not written a function that meets that declaration since it only returns Int.
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

-- 8: No. The new type declaration is saying the function must be able to return anything implementing Num, but we are only returning Int
sigmund' :: Int -> Int
sigmund' x = myX

-- 9: Yes. The new definition restricts what the function can operate on, but does not invalidate anything internal to the function.
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10: Yes. We are making the definition more generic this time, but the function contents are still valid.
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11: No. The `mySort` function used by signifier can only operate on [Char], but we're trying to use it on (Ord a => [a]).
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- Set 5:
-- 1:
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (f x) == y

-- 2:
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n x = n' * (f x) where n' = fromIntegral n :: Num c => c

-- Test ------------------------------------------------------------------------
-- (+) :: (Integral a, Fractional b) => a -> b -> b
-- (+) i f = f Prelude.+ fromIntegral i

-- (+) :: (Fractional a, Integral b) => a -> b -> a
-- (+) f i = f Prelude.+ fromIntegral i

-- End Test --------------------------------------------------------------------
