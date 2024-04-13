-- Traversable

module HB.Ch21 where

import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Data.ByteString.Lazy hiding (map)
import Data.Foldable (fold)
import Data.Monoid (Product (Product), Sum (Sum))
import HB.Morse (Morse, charToMorse)
import Network.Wreq
import Test.QuickCheck (Arbitrary (arbitrary), frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (foldable, functor, traversable)

-- 21.2: The Typeclass ---------------------------------------------------------

-- Note vague similarity to fmap:
-- fmap     :: (Functor f)     => (a -> b) -> f a -> f b
-- traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)

ex1 = fmap pure [1, 2, 3] :: [IO Int]

ex2 = traverse pure [1, 2, 3] :: IO [Int]

-- Counter to `traverse`
-- sequence: :: (Applicative f) => t (f a) -> f (t a)

-- 21.3: sequenceA -------------------------------------------------------------

sa1 = sequenceA [Just 1, Just 2, Just 4]

sa2 = sequenceA [Just 2, Nothing, Just 8]

sa3 = sequenceA $ Just $ Sum 5

-- 21.4: traverse --------------------------------------------------------------

tv :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
tv f = sequenceA . fmap f

-- 21.6: Morse Code Examples ---------------------------------------------------

stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse

-- NOTE: reading the source for traverse and sequenceA is not very helpful because they're defined in terms of the other.
-- But once a type implements the typeclass and provides one, each is well-defined.
-- This is the same kind of pattern I find myself wanting in C++ classes and hacking in through a brittle use of CRTP.

stringToFullMorse :: String -> Maybe String
stringToFullMorse s = fold <$> traverse charToMorse s

-- Double composition
-- dc = (iTakeOneArgument .) . iTakeTwoArguments

-- I just want to get to some exercises
-- Actually, I really just want to get to Ch23.

-- 21.7 ------------------------------------------------------------------------

-- nice

-- 21.8: HTTP?? ----------------------------------------------------------------

someURLs :: [String]
someURLs = ["http://httpbin.org/ip", "http://httpbin.org/bytes/5"]

mg :: [IO (Response ByteString)]
mg = map get someURLs

tg :: IO [Response ByteString]
tg = traverse get someURLs

-- 21.9: Traversable Instances -------------------------------------------------

-- Seeing the Either instances of Functor and Applicative right beside each
-- other makes it super clear how they're similar and different. In Functor, the
-- Either type is on the right of <$>, having some known function applied to it,
-- and in Applicative, the Either type is on the left of <*> , and maybe a
-- function is applied to some given RHS value.

ft = fmap Just ("wew", [1, 2, 3])

tt = traverse Just ("wew", [1, 2, 3])

-- 21.10: Traversable Laws -----------------------------------------------------

-- 1: Naturality
-- t . traverse f = traverse (t . f)

-- 2: Identity
-- traverse Identity = Identity
-- This is mostly what's been happening in all my tests using Just.

-- 3: Composition
-- traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f

-- 21.11: Testing --------------------------------------------------------------

testTest = quickBatch $ traversable (undefined :: [(Maybe Rational, Either (Sum Int) Integer, Double, Product Integer)])

-- 21.12: Exercises!!!!!!!!! ---------------------------------------------------

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Foldable Identity where
  foldr f c (Identity x) = f x c
  foldMap f (Identity x) = f x

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

testTrvId = quickBatch $ traversable (undefined :: Identity (Maybe Rational, Either (Sum Int) Integer, Double, Product Integer))

-- Constant
newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Foldable (Constant a) where
  foldr f c (Constant x) = c
  foldMap f (Constant x) = mempty

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = liftM Constant arbitrary

testFldConst = quickBatch $ foldable (undefined :: Constant Double (Integer, String, Sum Integer, Int, Int))

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

testFtrConst = quickBatch $ functor (undefined :: Constant Float (String, Int, Rational))

instance Traversable (Constant a) where
  traverse f (Constant x) = pure $ Constant x

testTrvConst = quickBatch $ traversable (undefined :: Constant Float (Maybe Rational, Either (Sum Int) Integer, Double, Product Integer))

-- Maybe
data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Foldable Optional where
  foldr f z Nada = z
  foldr f z (Yep x) = f x z

  foldMap f Nada = mempty
  foldMap f (Yep x) = f x

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = frequency [(3, liftM Yep arbitrary), (1, return Nada)]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

testTrvOpt = quickBatch $ traversable (undefined :: Optional (Maybe Rational, Either (Sum Int) Integer, Double, Product Integer))

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Foldable List where
  foldr f z Nil = z
  foldr f z (Cons x l) = f x $ Prelude.foldr f z l

  foldMap f Nil = mempty
  foldMap f (Cons x l) = mappend (f x) (foldMap f l)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [(4, liftM2 Cons arbitrary arbitrary), (1, return Nil)]

testFldList = quickBatch $ foldable (undefined :: List (Integer, String, Sum Integer, Int, Int))

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x l) = Cons (f x) (fmap f l)

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

testFtrList = quickBatch $ functor (undefined :: List (String, Int, Rational))

instance Traversable List where
  -- 1: Convert list of Applicatives values to list of Applicative "append value to list" functions (fmap (fmap Cons) l)
  -- 2: Sequentially apply functions with Nil base case (foldr (<*>) (pure Nil))

  -- sequenceA l = Prelude.foldr (<*>) (pure Nil) $ fmap (fmap Cons) l
  sequenceA = Prelude.foldr (\fx l -> fmap Cons fx <*> l) (pure Nil)

testTrvList = quickBatch $ traversable (undefined :: List (Maybe Rational, Either (Sum Int) Integer, Double, Product Integer))

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f c (Three _ _ x) = f x c

  foldMap f (Three _ _ x) = f x

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftM3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

testTrvThree = quickBatch $ traversable (undefined :: Three Float Double (Maybe Rational, Either (Sum Int) Integer, Double, Product Integer))

-- Pair
data Pair a b = Pair a b deriving (Eq, Show)

instance Foldable (Pair a) where
  foldr f c (Pair _ y) = f y c

  foldMap f (Pair _ y) = f y

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Traversable (Pair a) where
  traverse f (Pair x y) = Pair x <$> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = liftM2 Pair arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

testTrvPair = quickBatch $ traversable (undefined :: Pair Double (Maybe Rational, Either (Sum Int) Integer, Double, Product Integer))

-- Big
data Big a b = Big a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = liftM3 Big arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance Foldable (Big a) where
  foldMap f (Big _ x y) = f x <> f y

testFldBig = quickBatch $ foldable (undefined :: Big Double (Integer, String, Sum Integer, Int, Int))

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

testFtrBig = quickBatch $ functor (undefined :: Big Float (String, Int, Rational))

instance Traversable (Big a) where
  traverse f (Big x y1 y2) = Big x <$> f y1 <*> f y2

testTrvBig = quickBatch $ traversable (undefined :: Big Double (Maybe Rational, Either (Sum Int) Integer, Double, Product Integer))

-- Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = liftM4 Bigger arbitrary arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance Foldable (Bigger a) where
  foldMap f (Bigger _ x y z) = f x <> f y <> f z

testFldBigger = quickBatch $ foldable (undefined :: Bigger Double (Integer, String, Sum Integer, Int, Int))

instance Functor (Bigger a) where
  fmap f (Bigger w x y z) = Bigger w (f x) (f y) (f z)

testFtrBigger = quickBatch $ functor (undefined :: Bigger Float (String, Int, Rational))

instance Traversable (Bigger a) where
  traverse f (Bigger w x y z) = Bigger w <$> f x <*> f y <*> f z

testTrvBigger = quickBatch $ traversable (undefined :: Bigger Double (Maybe Rational, Either (Sum Int) Integer, Double, Product Integer))

-- S
data S n a = S (n a) a deriving (Eq, Show)

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = liftM2 S arbitrary arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

instance (Foldable n) => Foldable (S n) where
  foldMap f (S nx x) = foldMap f nx <> f x

testFldS = quickBatch $ foldable (undefined :: S Sum (Integer, String, Sum Integer, Int, Int))

instance (Functor n) => Functor (S n) where
  fmap f (S nx x) = S (fmap f nx) (f x)

testFtrS = quickBatch $ functor (undefined :: S Maybe (String, Int, Rational))

instance (Traversable n) => Traversable (S n) where
  -- traverse :: (Applicative f) => (a -> f b) -> S n a -> f (S n b)
  traverse f (S nx x) = S <$> traverse f nx <*> f x

testTrvS = quickBatch $ traversable (undefined :: S Product (Maybe Rational, Either (Sum Int) Integer, Double, Product Integer))

-- Tree
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = frequency [(1, liftM3 Node arbitrary arbitrary arbitrary), (1, liftM Leaf arbitrary), (1, return Empty)]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node tl x tr) = foldMap f tl <> f x <> foldMap f tr

testFldTree = quickBatch $ foldable (undefined :: Tree (Integer, String, Sum Integer, Int, Int))

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node tl x tr) = Node (fmap f tl) (f x) (fmap f tr)

testFtrTree = quickBatch $ functor (undefined :: Tree (String, Int, Rational))

instance Traversable Tree where
  -- traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node tl x tr) = Node <$> traverse f tl <*> f x <*> traverse f tr

testTrvTree = quickBatch $ traversable (undefined :: Tree (Maybe Rational, Either (Sum Int) Integer, Double, Product Integer))

-- Final Notes -----------------------------------------------------------------
-- The implementations of Traverse tend to boil down to fmapping the constructor over the result of function application:
-- traverse f (MyType x) = fmap MyType (f x)
-- If the constructor has more than one argument, that's where the Applicative <*> comes in.
-- traverse f (TwoThings x y) = TwoThings <$> f x <*> f y
-- Depending on the type signature and whether both x and y are relevant to the "f".
