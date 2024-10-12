module HB.Ch25 where

import Data.Complex
import Data.Monoid (Product (Product), Sum (Sum))
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Checkers (EqProp, eq, quickBatch, (=-=))
import Test.QuickCheck.Classes (applicative, foldable, functor, traversable)

-- 25.2: Common functions as types ---------------------------------------------

newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Show)

newtype Compose f g a = Compose {getCompose :: f (g a)} deriving (Eq, Show)

t1 = Compose [Just (1 :: Int), Nothing]

-- 25.3: Composing Functors ----------------------------------------------------

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- | We've seen this kind of thing before with a double fmap, but composition is letting us treat f (g a) more like fg a.
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

t2 = fmap (+ 1) t1

-- | Composition pattern reduced to single argument
newtype One f a = One (f a) deriving (Eq, Show)

instance (Functor f) => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

-- | Composition pattern extended to three arguments
newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

-- | Functors are "closed under composition."
t3 = Compose [Just (Compose $ Just [1 :: Int])]

-- 25.4: Composing Applicatives ------------------------------------------------

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  (Compose fgf) <*> (Compose fgx) = Compose $ fmap (<*>) fgf <*> fgx

t4 = Compose [Just (* 2), Nothing, Just (+ 3)]

t5 = t4 <*> t1

instance (Arbitrary (f (g a))) => Arbitrary (Compose f g a) where
  arbitrary = Compose <$> arbitrary

instance (Eq (f (g a))) => EqProp (Compose f g a) where
  (=-=) = eq

testComposeApplicative = quickBatch $ applicative (undefined :: Compose [] Maybe (Int, Double, Rational))

-- 25.5: Composing Monads ------------------------------------------------------

-- The `bind` definition hits a wall trying to reduce an (f (g (f (g a)))) to a (f (g a)).

-- 25.6: Exercises -------------------------------------------------------------

-- | 1.
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fgx) = foldMap (foldMap f) fgx

testComposeFoldable = quickBatch $ foldable (undefined :: Compose [] Maybe (Int, Complex Double, Product Int, Rational, Integer))

-- | 2. (a -> k b) -> Compose f g a -> k (Compose f g b)
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fgx) = Compose <$> (traverse . traverse) f fgx

testComposeTraversable = quickBatch $ traversable (undefined :: Compose [] Maybe (Maybe Rational, Either (Sum Int) Integer, Double, Product Integer))

-- Bifunctor
class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second g = bimap id g

-- 1
data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where
  bimap f g (Deux x y) = Deux (f x) (g y)

-- 2
data Const a b = Const a deriving (Eq, Show)

instance Bifunctor Const where
  bimap f _ (Const x) = Const (f x)

-- 3
data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei k x y) = Drei k (f x) (g y)

-- 4
data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei x y) = SuperDrei x (f y)

-- 5
data SemiDrei a b c = SemiDrei a deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei x) = SemiDrei x

-- 6
data Quadriceps a b c d = Quadriceps a b c d deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadriceps w x y z) = Quadriceps w x (f y) (g z)

-- 7
data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
  bimap f _ (Left' x) = Left' $ f x
  bimap _ g (Right' x) = Right' $ g x

-- 25.7: Monad Transformers ----------------------------------------------------

-- Nested bind is possible if one monad is known?

-- 25.8: IdentityT -------------------------------------------------------------

newtype IdentityT f a = IdentityT {runIdentityT :: f a} deriving (Eq, Show)

instance (Functor f) => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance (Applicative a) => Applicative (IdentityT a) where
  pure = IdentityT . pure

  (IdentityT ff) <*> (IdentityT fx) = IdentityT $ ff <*> fx

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  -- (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  -- The implementation details of simply wrapping q/q' back into an Identity and, I think, the fact that we use runIdentity to unpack the intermediate result are where the essense of the IdentityT monad are.
  (IdentityT fx) >>= f = IdentityT $ fx >>= runIdentityT . f
    where
      t = fmap f fx
      q = t >>= (\(IdentityT x) -> x) -- This is where we get specific about "what" the outer composed Monad is.
      ff = runIdentityT . f
      q' = fx >>= ff
