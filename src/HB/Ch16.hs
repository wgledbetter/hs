-- Functor
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module HB.Ch16 where

import Control.Monad
import Test.QuickCheck

-- 16.3 ------------------------------------------------------------------------

-- Just
a1 = fmap (+ 1) (Just 2)

-- Tuple
a2 = fmap (10 /) (4, 5)

-- 16.4 ------------------------------------------------------------------------

-- The type that implements functor must have kind (* -> *).
-- Otherwise the type signature ((a -> b) -> f a -> f b) wouldn't be possible.

-- Exercises
-- 1: *
-- 2: (* -> *) and (* -> *)
-- 3: * -> * -> *

-- <$> is infix for fmap

-- Functor instances can't apply an argument to the kind (* -> *).
-- e.g. "instance Functor (MyContainer x)" is wrong, but "instance Functor MyContainer" is correct.

-- 16.5: Functor Laws ----------------------------------------------------------

-- fmap id == id
-- fmap (f . g) == fmap f . fmap g

functorId :: (Functor f, Eq (f a)) => f a -> Bool
functorId fx = fmap id fx == id fx

functorComp :: (Functor f, Eq (f c)) => f a -> (a -> b) -> (b -> c) -> Bool
functorComp fx g h = fmap (h . g) fx == (fmap h . fmap g) fx

-- 16.6 ------------------------------------------------------------------------
-- There's nothing wrong with breaking the above laws, that's just not a functor.

-- 16.7: Common Functors -------------------------------------------------------

lms = [Just "woohoo", Nothing, Just "Ave"]

cp = const 'p'

q1 = fmap cp lms

q2 = (fmap . fmap) cp lms

q3 = (fmap . fmap . fmap) cp lms

-- Exercises:
-- 1:
a = fmap (+ 1) $ read "[1]" :: [Int]

-- 2:
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3:
c = fmap (* 2) (\x -> x - 2)

-- 4:
d = fmap ((return '1' ++) . show) (\x -> [x, 1 .. 3])

-- 5:
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = fmap read $ fmap ("123" ++) $ fmap show ioi
   in fmap (* 3) changed

-- 16.8: Transforming the unapplied type argument ------------------------------
-- The type that can have an instance of functor is of kind * -> *, but plenty of types have kind * -> * -> *, such as tuple or Either.
-- We can still create partial Functor instances for these types by pre-applying the first type argument and obtaining a kind-curried(?) type * -> *.
-- e.g. "instance Functor (Either a) where"
-- This means the functor only works over the second type variable though.

-- 16.10: Exercises ------------------------------------------------------------
-- 1:
newtype Identity a = Identity a deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

qc11 = quickCheck (functorId :: Identity String -> Bool)

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f x y z = f z y x

qc12 = quickCheck (flip3 functorComp (* 2) (fromIntegral :: Int -> Double) :: Identity Int -> Bool)

-- 2:
data Pair a = Pair a a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = liftM2 Pair arbitrary arbitrary

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

qc21 = quickCheck (functorId :: Pair Double -> Bool)

qc22 = quickCheck (flip3 functorComp (* 2) (fromIntegral :: Int -> Double) :: Pair Int -> Bool)

-- 3:
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM2 Two arbitrary arbitrary

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

qc31 = quickCheck (functorId :: Two String Int -> Bool)

qc32 = quickCheck (flip3 functorComp (* 2) (fromIntegral :: Int -> Double) :: Two String Int -> Bool)

-- 4:
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftM3 Three arbitrary arbitrary arbitrary

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

qc41 = quickCheck (functorId :: Three Double String Rational -> Bool)

qc42 = quickCheck (flip3 functorComp (/ 8) (+ 7) :: Three Double String Rational -> Bool)

-- 5:
data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftM3 Three' arbitrary arbitrary arbitrary

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

qc51 = quickCheck (functorId :: Three' String Rational -> Bool)

qc52 = quickCheck (flip3 functorComp (/ 8) (* 4) :: Three' String Rational -> Bool)

-- 6:
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = liftM4 Four arbitrary arbitrary arbitrary arbitrary

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

qc61 = quickCheck (functorId :: Four Bool Rational Double String -> Bool)

qc62 = quickCheck (flip3 functorComp (+ 7) length :: Four Bool Rational Double String -> Bool)

-- 7:
data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = liftM4 Four' arbitrary arbitrary arbitrary arbitrary

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

qc71 = quickCheck (functorId :: Four' Double Rational -> Bool)

qc72 = quickCheck (flip3 functorComp (+ 7) length :: Four' Double String -> Bool)

-- 8:
-- Kind is *.

-- 16.11: Ignoring Possibilities -----------------------------------------------

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

-- The fact that functor instances only apply to the right-most type argument means that any values not of that particular type are propagated through fmap applications unchanged.
-- In the case of Either, this means values of the first type (often error indicators) are robust to operations intended for the second type:

liftedAdd2 = fmap (+ 2)

data SnakeEyes = SnakeEyes deriving (Show, Eq)

type DoubleOrNothing = Either SnakeEyes Double

testFail = liftedAdd2 (Left SnakeEyes) -- 2 is not added, but it also doesn't crash trying to add 2 to SnakeEyes.

testSucc = liftedAdd2 (Right 12.5) -- Same function and types as above, but the fmap applies the addition.

-- Exercises
-- 1
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- 2: Impossible because of kind signature. The only way to reduce (* -> * -> *) to (* -> *) is by applying the first type argument.

-- 16.12: Surprises??? ---------------------------------------------------------

data MyConst a b = MyConst a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (MyConst a b) where
  arbitrary = liftM MyConst arbitrary

-- Functor acts on "phantom" type b that never manifests as type values.
instance Functor (MyConst a) where
  fmap _ (MyConst x) = MyConst x

-- Similar concept to above, where the error values can propagate past other-typed operations
mc1 = fmap (* 24) (MyConst "nah")

qcmc1 = quickCheck (functorId :: MyConst Rational String -> Bool)

-- The arbitrary value will only ever contain a rational, so the application of "length," while it must align with the type of (+6), never gets applied to any actual values of the MyConst type.
qcmc2 = quickCheck (flip3 functorComp (+ 6) length :: MyConst Rational String -> Bool)

-- 16.13: More Structure -------------------------------------------------------

data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance (Functor f) => Functor (Wrap f) where
  fmap f (Wrap wf) = Wrap (fmap f wf)

-- 16.14: IO -------------------------------------------------------------------

getInt :: IO Int
getInt = fmap read getLine

-- 16.15: Natural Transformations ----------------------------------------------
-- Instead of preserving type structure and changing the inhabiting values, natural transformations preserve the values and change the type structure.

-- Requires RankNTypes language extension
type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe [] -- Converts a Maybe which can contain anything into a list containing that thing
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 16.16: Uniqueness -----------------------------------------------------------
-- Types can have different valid Monoid instances, but only one valid Functor instance.

-- 16.17: Chapter Exercises ----------------------------------------------------
-- 1: No. Kind is *
-- 2: Yes
-- 3: Yes
-- 4: Yes, based on kind * -> *
-- 5: No.

-- 1: data Sum b a = First a | Second b
-- The functor instance is doing the fmap over the First constructor, so the type of the value in the First constructor needs to be available to the functor instance.
-- In the initial formulation, the value for Second was available, so I flipped the type arguments in the type definition.

-- 2: data Company a c b = DeepBlue a c | Something b
-- Similar rationale to above.

-- 3: data More b a = L a b a | R b a b
-- Same. Look at the instance and see where the function is getting applied. That type should be the final argument.

-- 1:
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ (Desk d) = Desk d
  fmap _ Finance = Finance

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = frequency [(3, liftM Bloor arbitrary), (3, liftM Desk arbitrary), (1, return Finance)]

qcex11 = quickCheck (functorId :: Quant Rational Double -> Bool)

qcex12 = quickCheck (flip3 functorComp (+ 6) length :: Quant Rational String -> Bool)

-- 2:
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = liftM K arbitrary

qcex21 = quickCheck (functorId :: K Rational String -> Bool)

qcex22 = quickCheck (flip3 functorComp (+ 6) length :: K Rational String -> Bool)

-- 3:
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))

instance (Arbitrary (f b a)) => Arbitrary (Flip f a b) where
  arbitrary = liftM Flip arbitrary

qcex31 = quickCheck (functorId :: Flip K Rational String -> Bool)

qcex32 = quickCheck (flip3 functorComp (+ 6) length :: Flip K Rational String -> Bool)

-- 4:
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = liftM GoatyConst arbitrary

qcex41 = quickCheck (functorId :: EvilGoateeConst Rational String -> Bool)

qcex42 = quickCheck (flip3 functorComp (+ 6) length :: EvilGoateeConst Rational String -> Bool)

-- 5:
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut fx) = LiftItOut (fmap f fx)

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
  arbitrary = liftM LiftItOut arbitrary

qcex51 = quickCheck (functorId :: LiftItOut [] Double -> Bool)

qcex52 = quickCheck (flip3 functorComp (+ 6) length :: LiftItOut [] String -> Bool)

-- 6:
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fx gx) = DaWrappa (fmap f fx) (fmap f gx)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
  arbitrary = liftM2 DaWrappa arbitrary arbitrary

qcex61 = quickCheck (functorId :: Parappa [] (K Rational) String -> Bool)

qcex62 = quickCheck (flip3 functorComp (+ 6) length :: Parappa [] (Flip K Rational) String -> Bool)

-- 7:
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fx gy) = IgnoringSomething fx (fmap f gy)

instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
  arbitrary = liftM2 IgnoringSomething arbitrary arbitrary

qcex71 = quickCheck (functorId :: IgnoreOne [] [] Rational String -> Bool)

qcex72 = quickCheck (flip3 functorComp (+ 6) length :: IgnoreOne (Flip K Rational) (K Rational) (Maybe Double) String -> Bool)

-- 8:
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) => Arbitrary (Notorious g o a t) where
  arbitrary = liftM3 Notorious arbitrary arbitrary arbitrary

qcex81 = quickCheck (functorId :: Notorious [] String Double Rational -> Bool)

qcex82 = quickCheck (flip3 functorComp (+ 6) length :: Notorious (K Double) Int Rational String -> Bool)

-- 9:
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x l) = Cons (f x) (fmap f l)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [(5, liftM2 Cons arbitrary arbitrary), (1, return Nil)]

qcex91 = quickCheck (functorId :: List Rational -> Bool)

qcex92 = quickCheck (flip3 functorComp (+ 9) length :: List String -> Bool)

-- 10:
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat g) = OneGoat (f g)
  fmap f (MoreGoats glx gly glz) = MoreGoats (fmap f glx) (fmap f gly) (fmap f glz)

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary =
    frequency
      [ (1, liftM3 MoreGoats arbitrary arbitrary arbitrary),
        (3, liftM OneGoat arbitrary),
        (2, return NoGoat)
      ]

qcex101 = quickCheck (functorId :: GoatLord Rational -> Bool)

qcex102 = quickCheck (flip3 functorComp (* 7) length :: GoatLord String -> Bool)

-- 11:
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read s2a) = Read (f . s2a)

-- Oh, and <$> is infix shorthand for fmap
