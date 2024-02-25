-- Monad

module HB.Ch18 where

import Control.Applicative (liftA2, liftA3)
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 18.2 ------------------------------------------------------------------------

-- Monad return is the same as Applicative pure.
-- (>>) Sequencing operator. "Chains" two actions, discarding first result. Also has equivalent in Applicative.
-- (>>=) bind. The important part.

-- join :: (Monad m) => m (m a) -> m a

myBind :: (Monad m) => (a -> m b) -> m a -> m b
myBind f = join . fmap f

-- Due to the requirement of a Monad having an Applicative instance, liftM is equivalent to liftA. Kinda like pure and return.
-- zipWith is equivalent to liftA2, just with the ZipList Monoid instead of the default outer-product-ish one.

-- Monad was used first, with Applicative being extracted later.

-- 18.3: do --------------------------------------------------------------------

-- `do` is just a nice way to chain (>>) or (*>).

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

-- 18.4: Examples --------------------------------------------------------------

-- List

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs -- This passes over every value in the list.
  if even x
    then [x ^ 2, x ^ 2]
    else []

-- Maybe

data Cow = Cow {name :: String, age :: Int, weight :: Int} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
   in if n == "Bess" && 500 <= w then Nothing else Just c

-- You have to have Monad for this post-creation weightCheck because weightCheck accepts a Cow, not a Maybe Cow.
mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow n a w = (liftA3 Cow (noEmpty n) (noNegative a) (noNegative w)) >>= weightCheck

-- Either

type Founded = Int

type Coders = Int

data SoftwareShop = Shop {founded :: Founded, programmers :: Coders} deriving (Eq, Show)

data ShopError
  = NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either ShopError Founded
validateFounded y
  | y < 0 = Left $ NegativeYears y
  | y > 500 = Left $ TooManyYears y
  | otherwise = Right y

validateCoders :: Int -> Either ShopError Coders
validateCoders c
  | c < 0 = Left $ NegativeCoders c
  | c > 5000 = Left $ TooManyCoders c
  | otherwise = Right c

validateShop :: SoftwareShop -> Either ShopError SoftwareShop
validateShop s =
  if prgs > div yrs 10
    then Left $ TooManyCodersForYears prgs yrs
    else Right s
  where
    prgs = programmers s
    yrs = founded s

mkSoftware :: Int -> Int -> Either ShopError SoftwareShop
mkSoftware y c = (liftA2 Shop (validateFounded y) (validateCoders c)) >>= validateShop

-- Exercise: Either Monad

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First x) _ = First x
  (<*>) _ (First x) = First x
  (<*>) (Second x) (Second y) = Second (x y)

instance Monad (Sum a) where
  return = pure
  (>>=) (First x) _ = First x
  (>>=) (Second x) f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary =
    frequency
      [ (1, liftM First arbitrary),
        (1, liftM Second arbitrary)
      ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

eitherMonadTest = quickBatch $ monad (undefined :: Sum (Rational) (Integer, String, Double))

eitherApplicativeTest = quickBatch $ applicative (undefined :: Sum (Rational) (Integer, String, Double))

eitherFunctorTest = quickBatch $ functor (undefined :: Sum (Rational) (Integer, String, Double))

-- 18.5: Monad Laws ------------------------------------------------------------

-- Right Identity
-- m >>= return == m

-- Left Identity
-- return x >>= f == f x

-- Associativity
-- (m >>= f) >>= g == m >>= (\x -> f x >>= g)

-- 18.6: Application and composition -------------------------------------------

-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

-- 18.7: Chapter Exercises -----------------------------------------------------

-- 1:
data Nope a = NopeDotJpeg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpeg

instance Applicative Nope where
  pure _ = NopeDotJpeg
  (<*>) _ _ = NopeDotJpeg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpeg

instance Arbitrary (Nope a) where
  arbitrary = frequency [(1, return NopeDotJpeg)]

instance EqProp (Nope a) where
  (=-=) = eq

ex1m = quickBatch $ monad (undefined :: Nope (Double, Rational, Integer))

ex1a = quickBatch $ applicative (undefined :: Nope (Double, Rational, Integer))

ex1f = quickBatch $ functor (undefined :: Nope (Double, Rational, Integer))

-- 2:
data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap f (PLeft x) = PLeft (f x)
  fmap _ (PRight y) = PRight y

instance Applicative (BahEither b) where
  pure = PLeft
  (<*>) (PRight x) _ = PRight x
  (<*>) _ (PRight x) = PRight x
  (<*>) (PLeft f) (PLeft x) = PLeft (f x)

instance Monad (BahEither b) where
  return = pure
  (>>=) (PRight x) _ = PRight x
  (>>=) (PLeft x) f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = frequency [(1, liftM PLeft arbitrary), (1, liftM PRight arbitrary)]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

ex2m = quickBatch $ monad (undefined :: BahEither String (Double, Rational, Integer))

ex2a = quickBatch $ applicative (undefined :: BahEither String (Double, Rational, Integer))

ex2f = quickBatch $ functor (undefined :: BahEither String (Double, Rational, Integer))

-- 3:
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

ex3m = quickBatch $ monad (undefined :: Identity (Double, Rational, Integer))

ex3a = quickBatch $ applicative (undefined :: Identity (Double, Rational, Integer))

ex3f = quickBatch $ functor (undefined :: Identity (Double, Rational, Integer))

-- 4:
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) (Cons x xs) ys = Cons x (xs <> ys)
  (<>) Nil ys = ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x l) = Cons (f x) (fmap f l)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f lf) lx = (f <$> lx) <> (lf <*> lx)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x l) f = (f x) <> (l >>= f)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [(5, liftM2 Cons arbitrary arbitrary), (1, return Nil)]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

ex4m = quickBatch $ monad (undefined :: List (Double, Rational, Integer))

ex4a = quickBatch $ applicative (undefined :: List (Double, Rational, Integer))

ex4f = quickBatch $ functor (undefined :: List (Double, Rational, Integer))

ex4s = quickBatch $ semigroup (undefined :: (List Rational, Integer))

-- Complete the functions

-- 1:
j :: (Monad m) => m (m a) -> m a
j = join

-- 2:
l1 :: (Monad m) => (a -> b) -> m a -> m b
l1 f ma = fmap f ma

-- 3:
l2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4:
a :: (Monad m) => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

-- 5: I cheated and looked at the GHC.Base definition of "mapM". That and "sequence" were the hoogle results for "[m a] -> m [a]".
-- Also, I don't _really_ understand what's going on under the hood when you do this double variable assignment-like thing inside "do" notation.
meh :: (Monad m) => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) f = do
  y <- f x
  ys <- meh xs f
  return (y : ys)

-- 6:
flipType :: (Monad m) => [m a] -> m [a]
flipType x = meh x (\y -> y >>= return)
