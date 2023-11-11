-- Type Classes

module HB.Ch6 where

-- The pattern where you derive from a typeclass which defines a set of
-- functions but you only have to overload/define a subset of all funtions feels
-- extremely similar to the CRTP base class pattern where two functions can
-- depend on each other in the base class and can call the opposite
-- implementation in the derived class. Like in the ASSET architecture how only
-- one of jacobian() and compute_jacobian() needs to be overloaded, but you get
-- the full functionality of both.

-- 6.5 Exercises: Eq
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

-- Test ------------------------------------------------------------------------
-- (+) :: (Integral a, Fractional b) => a -> b -> b
-- (+) i f = f Prelude.+ fromIntegral i

-- (+) :: (Fractional a, Integral b) => a -> b -> a
-- (+) f i = f Prelude.+ fromIntegral i

-- End Test --------------------------------------------------------------------
