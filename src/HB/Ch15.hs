-- Monoid and Semigroup

module HB.Ch15 where

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
