-- Monoid and Semigroup

module HB.Ch15 where

import Control.Monad -- Cheating and kind of skipping ahead
import Data.List.NonEmpty
import Data.Monoid
import Test.QuickCheck

-- 15.3: Monoid ----------------------------------------------------------------
-- "A monoid is a binary associative operation with an identity."
-- Function with two arguments (order doesn't matter) such that there exists an argument A that will return B for any second argument B?

-- 15.4: Monoids in Haskell ----------------------------------------------------
-- mempty = the identity value
-- mappend = the singular operation (also <> from Semigroup)
-- mconcat = foldr mappend mempty

-- 15.5: Examples --------------------------------------------------------------
-- Why are lists monoids? Isn't the append operation not associative?
--   Oh, associativity is (x+y) + z == x + (y+z).
-- "Lists form a monoid under concatenation"

-- 15.6: Integers --------------------------------------------------------------
-- The integers have two monoids: addition and multiplication. Which should be the "default"?
-- mappend (Sum 1) (Sum 4)
-- mappend (Product 3) (Product 7)
-- using a `newtype` wrapper is a good way to separate multiple monoids for any type

someSum = getSum $ Sum 12 <> Sum 1 <> Sum (-4)

-- 15.7: But Why? --------------------------------------------------------------
-- Abelian/Commutative Monoid

-- 15.8: Laws ------------------------------------------------------------------

-- 15.9: -----------------------------------------------------------------------

-- Last Maybe Monoid

l1 = Last (Just 5)

l2 = Last Nothing

l3 = Last (Just 4)

ll1 = l1 <> l2 -- Just 5 is the "last" non-nothing value

ll2 = l2 <> l3 -- Just 4

ll3 = l1 <> l3 <> l2 -- Just 4

-- First Maybe Monoid

f1 = First (Just 7)

f2 = First (Nothing)

f3 = First (Just 1)

ff1 = f1 <> f2 -- Just 7

ff2 = f2 <> f3 -- Just 1

ff3 = f1 <> f2 <> f3 -- Just 7

-- 15.10: Reusing Algebras -----------------------------------------------------

data Optional a = Nada | Only a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Optional a) where
  (<>) Nada x = x
  (<>) x Nada = x
  (<>) (Only x) (Only y) = Only (x <> y)

instance (Monoid a) => Monoid (Optional a) where
  mempty = Nada
  mappend x Nada = x
  mappend Nada x = x
  mappend (Only x) (Only y) = Only (mappend x y)

-- Orphan Instance: duplicate typeclass instance defined separately from datatype
-- see above. use newtype wrapper.
-- If they were defined in the same module, it would be a compile error.

-- 15.11: MADNESS --------------------------------------------------------------

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbinBetter :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter e adv noun adj =
  mconcat
    [ e,
      "! he said ",
      adv,
      " as he jumped into his ",
      noun,
      " and drove off with his ",
      adj,
      " wife."
    ]

-- 15.12: Validating laws with QuickCheck --------------------------------------

assoc :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
assoc (<>) a b c = a <> (b <> c) == (a <> b) <> c

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc = assoc (<>)

qqa = quickCheck (monoidAssoc :: (String -> String -> String -> Bool))

vva = verboseCheck (monoidAssoc :: (String -> String -> String -> Bool))

monoidLeftIdent :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdent a = (mempty <> a) == a

monoidRightIdent :: (Eq m, Monoid m) => m -> Bool
monoidRightIdent a = (a <> mempty) == a

-- QuickCheck uses the "Arbitrary" typeclass to generate inputs
-- This section is basically saying just because you use the word "monoid" as the name of a type class, that doesn't mean the definitions you've provided are actually monoid-al.

-- Exercise: Maybe another Monoid

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' (Only x)) _ = First' (Only x)
  (<>) _ b = b

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = frequency [(5, liftM (First' . Only) arbitrary), (1, return (First' Nada))]

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

mck1 = quickCheck (monoidAssoc :: FirstMappend)

mck2 = quickCheck (monoidLeftIdent :: FstId)

mck3 = quickCheck (monoidRightIdent :: FstId)

-- 15.13: Semigroup ------------------------------------------------------------
-- Just remove the identity from Monoid.

nel = 1 :| [2, 3]

hn = Data.List.NonEmpty.head nel

h2 = nel Data.List.NonEmpty.!! 1

-- 15.14: Algebra Strength -----------------------------------------------------
-- has more ops

-- 15.15: Exercises ------------------------------------------------------------
-- Semigroup
-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc x y z = (x <> (y <> z)) == ((x <> y) <> z)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

ex11 = quickCheck (semigroupAssoc :: TrivAssoc)

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (<>) (Identity l) (Identity r) = Identity (l <> r)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

type IdentAssoc = Identity String -> Identity String -> Identity String -> Bool

ex12 = quickCheck (semigroupAssoc :: IdentAssoc)

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x y) (Two i j) = Two (x <> i) (y <> j)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM2 Two arbitrary arbitrary

type TwoAssoc = Two (Sum Int) (Product Int) -> Two (Sum Int) (Product Int) -> Two (Sum Int) (Product Int) -> Bool

ex13 = quickCheck (semigroupAssoc :: TwoAssoc)

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three a b c) (Three x y z) = Three (a <> x) (b <> y) (c <> z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftM3 Three arbitrary arbitrary arbitrary

type ThreeAssoc =
  Three String (Sum Rational) (Product Rational) ->
  Three String (Sum Rational) (Product Rational) ->
  Three String (Sum Rational) (Product Rational) ->
  Bool

ex14 = quickCheck (semigroupAssoc :: ThreeAssoc)

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four w x y z) = Four (a <> w) (b <> x) (c <> y) (d <> z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = liftM4 Four arbitrary arbitrary arbitrary arbitrary

type FourAssoc =
  Four String (Product Rational) (Maybe (Sum Int)) (First (Maybe Double)) ->
  Four String (Product Rational) (Maybe (Sum Int)) (First (Maybe Double)) ->
  Four String (Product Rational) (Maybe (Sum Int)) (First (Maybe Double)) ->
  Bool

ex15 = quickCheck (semigroupAssoc :: FourAssoc)

-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj y) = BoolConj y
  (<>) (BoolConj x) (BoolConj True) = BoolConj x
  (<>) _ _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = frequency [(1, return (BoolConj True)), (1, return (BoolConj False))]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

ex16 = quickCheck (semigroupAssoc :: BoolConjAssoc)

-- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj True) _ = BoolDisj True
  (<>) _ (BoolDisj True) = BoolDisj True
  (<>) _ _ = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = frequency [(1, return (BoolDisj True)), (1, return (BoolDisj False))]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

ex17 = quickCheck (semigroupAssoc :: BoolDisjAssoc)

-- 8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Snd x) _ = Snd x
  (<>) (Fst _) (Snd y) = Snd y
  (<>) _ y = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, liftM Fst arbitrary), (1, liftM Snd arbitrary)]

type OrAssoc =
  Or String (Product Rational) ->
  Or String (Product Rational) ->
  Or String (Product Rational) ->
  Bool

ex18 = quickCheck (semigroupAssoc :: OrAssoc)

-- 9
newtype Combine a b = Combine {unCombine :: (a -> b)}

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (\x -> (f x) <> (g x))

-- instance (CoArbitrary a,Arbitrary b) => Arbitrary (Combine a b) where
--   arbitrary = liftM Combine arbitrary

-- type CombineAssoc =
--   Combine Int (Sum Int) ->
--   Combine Int (Sum Int) ->
--   Combine Int (Sum Int) ->
--   Bool

-- ex19 = quickCheck (semigroupAssoc :: CombineAssoc)

-- 10
newtype Comp a = Comp {unComp :: (a -> a)}

instance Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp $ f . g

-- 11
data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) where
  (<>) (Success' x) _ = Success' x
  (<>) _ (Success' y) = Success' y
  (<>) (Failure' x) (Failure' y) = Failure' (x <> y)

-- Monoid Exercises
-- 1
instance Monoid Trivial where
  mempty = Trivial

type TrivId = Trivial -> Bool

ex21l = quickCheck (monoidLeftIdent :: TrivId)

ex21r = quickCheck (monoidRightIdent :: TrivId)

-- 2
instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

type IdentId = Identity String -> Bool

ex22l = quickCheck (monoidLeftIdent :: IdentId)

ex22r = quickCheck (monoidRightIdent :: IdentId)

-- 3
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

type TwoId = Two (Sum Int) (Product Rational) -> Bool

ex23l = quickCheck (monoidLeftIdent :: TwoId)

ex23r = quickCheck (monoidRightIdent :: TwoId)

-- 4
instance Monoid BoolConj where
  mempty = BoolConj True

type BoolConjId = BoolConj -> Bool

ex24l = quickCheck (monoidLeftIdent :: BoolConjId)

ex24r = quickCheck (monoidRightIdent :: BoolConjId)

-- 5
instance Monoid BoolDisj where
  mempty = BoolDisj False

type BoolDisjId = BoolDisj -> Bool

ex25l = quickCheck (monoidLeftIdent :: BoolDisjId)

ex25r = quickCheck (monoidRightIdent :: BoolDisjId)

-- 6
instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)

-- 7
instance Monoid (Comp a) where
  mempty = Comp id

-- 8
newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance (Semigroup a) => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) = Mem h
    where
      h s = ((fst fs) <> (fst gs), (snd . f . snd) gs)
        where
          fs = f s
          gs = g s

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
