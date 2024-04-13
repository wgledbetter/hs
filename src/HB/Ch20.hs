-- Foldable

module HB.Ch20 where

import Control.Monad
import qualified Data.Foldable as F (fold, foldMap)
import Data.Monoid (First, Product, Sum)
import HB.Ch15 (Optional (Nada, Only))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 20.3: Revenge of the Monoids ------------------------------------------------

t1 = F.fold [4, 5] :: (Sum Integer)

t2 = F.fold [4, 5] :: (Product Integer)

-- The folding functions we've used haven't required a Monoid on the contained type because we pass the `mappend` function explicitly as an argument to foldl and foldr.
-- In fact, I'd guess that foldl and foldr allow that so you can use functions that may not be associative.
-- You also pass in a sort of `mempty` equivalent, which is intuitively thought of as the "starting value."

-- 20.4: Demonstrating Foldable Instances --------------------------------------

data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z

  foldMap f (Identity x) = f x

instance Foldable Optional where
  foldr f z Nada = z
  foldr f z (Only x) = f x z

  foldMap f Nada = mempty
  foldMap f (Only x) = f x

-- 20.5: Derived Ops -----------------------------------------------------------

-- toList
-- null
-- length
-- elem
-- maximum
-- minimum
-- sum
-- product

-- Exercises: Library Functions

-- 1
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

-- 2
product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

-- 3
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e = foldr (\x b -> (x == e) || b) False

-- 4
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum =
  foldr
    ( \x m -> case fmap (< x) m of
        Just b -> if b then m else Just x
        Nothing -> Just x
    )
    Nothing

-- 5
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum =
  foldr
    ( \x m -> case fmap (< x) m of
        Just b -> if b then m else Just x
        Nothing -> Just x
    )
    Nothing

-- 6
null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

-- 7
length :: (Foldable t) => t a -> Int
length = foldr (\_ c -> c + 1) 0

-- 8
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- 9
fold :: (Foldable t, Monoid m) => t m -> m
fold = F.foldMap id

-- 10
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (\x a -> mappend (f x) a) mempty

-- 20.6: Chapter Exercises -----------------------------------------------------

-- 1
data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f c (Constant x) = f x c

  foldMap f (Constant x) = f x

instance (Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = liftM Constant arbitrary

t1f = quickBatch $ foldable (undefined :: Constant Double (Integer, String, Sum Integer, Int, Int))

-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f c (Two _ y) = f y c

  foldMap f (Two _ y) = f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM2 Two arbitrary arbitrary

t2f = quickBatch $ foldable (undefined :: Two Integer (String, Integer, Product Int, Rational, Double))

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f c (Three _ _ x) = f x c

  foldMap f (Three _ _ x) = f x

-- 4
data Three' a b = Three' a b b deriving (Eq, Show)

-- NOTE(wgl): This passes tests, but the implementation is not unique. Can also use the commented lines.
-- NOTE(wgl): SEE Ch21:266 FOR CORRECT IMPLEMENTATION
instance Foldable (Three' a) where
  -- foldr f c (Three' _ x y) = f x c
  foldr f c (Three' _ x y) = f y c

  -- foldMap f (Three' _ x y) = f x
  foldMap f (Three' _ x y) = f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftM3 Three' arbitrary arbitrary arbitrary

t4f = quickBatch $ foldable (undefined :: Three' Rational (Double, String, Product Integer, Rational, Double))

-- 5
data Four' a b = Four' a b b b deriving (Eq, Show)

-- NOTE(wgl): Incorrect. See Ch21 "Bigger"
instance Foldable (Four' a) where
  foldr f c (Four' _ _ _ x) = f x c

  foldMap f (Four' _ _ _ x) = f x

-- Bonus
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF fn = F.foldMap (\x -> if fn x then pure x else mempty)

tf1 = filterF (== 4) [1, 2, 3, 4, 5] :: [Int]

tf2 = filterF (< 3) [1, 2, 3, 4, 5] :: Sum Int

tf3 = filterF (> 6) [1, 2, 3, 4, 5] :: First Int
