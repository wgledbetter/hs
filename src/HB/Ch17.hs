-- Applicative

module HB.Ch17 where

import Control.Applicative
import Control.Monad
import Data.List (elemIndex)
import Data.Monoid
import HB.Ch15 (Validation (Failure', Success'))
import HB.Ch16
  ( Four (Four),
    Four' (Four'),
    List (Cons, Nil),
    Pair (Pair),
    Three (Three),
    Three' (Three'),
    Two (Two),
  )
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 17.2: Definition ------------------------------------------------------------

-- class (Functor f) => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- 17.3: Functor vs. Applicative -----------------------------------------------

-- fmap  ::   (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

-- fmap f x == pure f <*> x
applicativeFunctor :: (Applicative f, Eq (f b)) => (a -> b) -> f a -> Bool
applicativeFunctor f x = fmap f x == (pure f <*> x)

-- Predicting ahead a bit, I think the usefulness of Applicative is going to show itself for functions with multiple arguments.
-- I think that by having "f (a -> b)", you'll be able to represent the intermediate application of functions, and chain those together to obtain a final value.
-- Or not. You could curry, too.
t1 = Just (+) <*> Just 8

t2 = t1 <*> Just 2

-- 17.4: -----------------------------------------------------------------------
-- Applicatives are monoidal functors.
-- Think of "mappend" being applied over the structure and fmap being applied over the values.

sample1 = [(* 2), (* 3)] <*> [4, 5] -- [2*4, 2*5, 3*4, 3*5]

sample2a = Just (* 2) <*> Just 2 -- Just 4

sample2b = Just (* 2) <*> Nothing -- Nothing

sample2c = Nothing <*> Just 2 -- Nothing

sample2d = Nothing <*> Nothing -- Nothing

-- The applicative instance for tuple only requires monoid for the first value.
-- The second value is where the function application happens.
-- Similar to Functor where only the last type argument can be manipulated.
sample3 = ("Woo", (+ 1)) <*> (" Hoo!", 0) -- Monoid required for first value in tuple

-- A tuple can only "have/be" a monoid if both values "have/are" a monoid.
-- A tuple can be an applicative even if only the first value "has/is" a monoid.

-- 17.5: Applicative in use ----------------------------------------------------

ex1 = (,) <$> [1, 2] <*> [3, 4] -- [(1,3), (1,4), (2,3), (2,4)]

ex2 = liftA2 (,) [1, 2] [3, 4] -- same as ex1

-- Cartesian product-ish?

ex3 = lookup 3 [(3, "Hello")]

-- Exercises

-- 1:
added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2:
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3:
x' :: Maybe Int
x' = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxxed :: Maybe Int
maxxed = max' <$> x' <*> y'

-- 4:
xs = [1, 2, 3]

ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')

-- Wrapping stuff in an Identity type can be an easy way to change behavior of <*>.

-- Exercise: Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

-- Exercise: Constant
newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance (Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant $ mappend a b

-- Maybe Applicative

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen then Nothing else Just s

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

-- First fmap Person over maybe name, then <*> over address.
mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

-- liftA2 X y z == X <$> y <*> z
-- liftA3 W x y z == W <$> x <*> y <*> z
-- etc.

-- Exercise: fixer upper

ex11 = const <$> Just "Hello" <*> (pure "World")

ex12 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- 17.6: Applicative Laws ------------------------------------------------------

applicativeIdentity :: (Applicative f, Eq (f a)) => f a -> Bool
applicativeIdentity a = (pure id <*> a) == a

applicativeComposition :: (Applicative f, Eq (f c)) => f (b -> c) -> f (a -> b) -> f a -> Bool
applicativeComposition fx fy fz =
  (pure (.) <*> fx <*> fy <*> fz) -- Combines fx and fy first
    == (fx <*> (fy <*> fz)) -- Applies fy to fz first

-- applicativeHomomorphism :: (Applicative f, Eq (f b)) => (a -> b) -> a -> Bool
-- applicativeHomomorphism func val = (pure func <*> pure val) == pure (func val)

applicativeInterchange :: (Applicative f, Eq (f b)) => f (a -> b) -> a -> Bool
applicativeInterchange fab x = (fab <*> pure x) == (pure ($ x) <*> fab)

-- 17.7: Testing Applicative Laws ----------------------------------------------

-- The 'monoid' function creates a TestBatch.
-- The actual value that's passed is irrelevant. It's only used to solidify types.
qb1 = quickBatch $ monoid ([4] :: [Int])

qb2 = quickBatch $ applicative ([(2.5, "c", 7)] :: [(Double, String, Integer)])

type SSI = (String, String, Int)

qb3 = quickBatch $ applicative (undefined :: [SSI])

-- 17.8: ZipList Monoid --------------------------------------------------------

instance (Semigroup a) => Semigroup (ZipList a) where
  (<>) a b = (<>) <$> a <*> b

instance (Monoid a) => Monoid (ZipList a) where
  mempty = pure mempty -- This produces an infinite list of memptys because that ensures that any (x <> mempty == x). A shorter mempty list could truncate x.

-- List Applicative

instance Semigroup (List a) where
  (<>) (Cons x xs) ys = Cons x (xs <> ys)
  (<>) Nil ys = ys

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f lf) lx = (f <$> lx) <> (lf <*> lx)

-- ZipList Applicative

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance (Semigroup a) => Semigroup (ZipList' a) where
  (<>) a b = (<>) <$> a <*> b

instance (Monoid a) => Monoid (ZipList' a) where
  mempty = pure mempty

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (repeat x)
  (<*>) (ZipList' []) _ = ZipList' []
  (<*>) _ (ZipList' []) = ZipList' []
  (<*>) (ZipList' (f : fs)) (ZipList' (x : xs)) = ZipList' ((f x) : appliedRest)
    where
      ZipList' appliedRest = (ZipList' fs <*> ZipList' xs)

-- Sample using the Validation type, which has an Applicative

instance Functor (Validation e) where
  fmap _ (Failure' f) = Failure' f
  fmap f (Success' x) = Success' (f x)

instance (Monoid e) => Applicative (Validation e) where
  pure = Success'
  (<*>) (Failure' f1) (Failure' f2) = Failure' (f1 <> f2)
  (<*>) (Failure' f) _ = Failure' f
  (<*>) _ (Failure' f) = Failure' f
  (<*>) (Success' s1) (Success' s2) = Success' (s1 s2)

-- 17.9: Chapter Exercises -----------------------------------------------------

-- 1:
-- pure1 :: a -> [a]
-- (<*>$) :: [(a -> b)] -> [a] -> [b]
-- 2:
-- pure2 :: a -> IO a
-- (<*>$$) :: IO (a -> b) -> IO a -> IO b
-- 3:
-- pure3 :: a -> (a, a)
-- (<*>$$$) :: (a, (a -> b)) -> (a, a) -> (a, b)
-- 4:
-- pure4 :: a -> (e -> a)
-- (<*>$$$$) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)

-- 1: Imported from Ch16
instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f1 f2) (Pair x1 x2) = Pair (f1 x1) (f2 x2)

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

ce1 = quickBatch $ applicative (undefined :: Pair (Double, Rational, String))

-- 2: Imported from Ch16
instance (Monoid a) => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two x1 y1) (Two x2 y2) = Two (x1 <> x2) (y1 y2)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

ce2 = quickBatch $ applicative (undefined :: Two (Product Rational) (Double, Integer, String))

-- 3: Imported from Ch16
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three x1 y1 z1) (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 z2)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

ce3 = quickBatch $ applicative (undefined :: Three (Sum Integer) ([Double]) (Rational, String, Double))

-- 4: Imported from Ch16
instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' x1 y1 z1) (Three' x2 y2 z2) = Three' (x1 <> x2) (y1 y2) (z1 z2)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

ce4 = quickBatch $ applicative (undefined :: Three' (Sum Int) (Rational, [Integer], String))

-- 5: Imported from Ch16
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four w1 x1 y1 z1) (Four w2 x2 y2 z2) = Four (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 z2)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

ce5 = quickBatch $ applicative (undefined :: Four (Sum Integer) (Product Int) (Sum Rational) (String, [Int], Product Double))

-- 6: Imported from Ch16
instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' w1 x1 y1 z1) (Four' w2 x2 y2 z2) = Four' (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 z2)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

ce6 = quickBatch $ applicative (undefined :: Four' (Product Rational) (Integer, [Double], String))

-- Combos:
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

-- Leverages the particular implementation of the [] Applicative as a sort of outer product
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
