-- Reader

module HB.Ch22 where

import Control.Applicative
import qualified Control.Monad.Trans.Reader as R
import Data.Char
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.Maybe
import HB.Ch15 (BoolConj (BoolConj))

-- 22.2 ------------------------------------------------------------------------

boob :: (Num a) => a -> a
boob = (* 2)

dood :: (Num a) => a -> a
dood = (+ 10)

bib :: (Num a) => a -> a
bib = boob . dood

did :: (Num a) => a -> a
did = fmap boob dood -- Map boob over dood

-- NOTE: type (->) :: * -> * -> *
-- thus, type (Int ->) :: * -> *, which is the necessary kind for a functor instance

bbop :: Integer -> Integer
bbop = (+) <$> boob <*> dood

duwop :: Integer -> Integer
duwop = liftA2 (+) boob dood

twoXfivePone = ((+) <$> boob) 5 1 -- 2*5 + 1

-- my attempt at implementing <*> for functions
fapply :: (a -> b -> c) -> (a -> b) -> a -> c
fapply f g x = f x (g x)

bbop2 :: (Num a) => a -> a
bbop2 = ((+) <$> boob) `fapply` dood

boobDood :: (Num a) => a -> a
boobDood = do
  a <- boob
  b <- dood
  return (a + b)

-- "it gives us a way to do computation in terms of an argument that hasn't been supplied yet"

-- Short Exercise

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupledA :: [Char] -> ([Char], [Char])
tupledA = (,) <$> cap <*> rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  c <- cap
  r <- rev
  return (c, r)

tupledB :: [Char] -> ([Char], [Char])
tupledB x = rev <$> (cap >>= (,)) x

-- 22.3 ------------------------------------------------------------------------

-- This is the pattern I've been waiting for.

-- 22.4: Functor of Functions --------------------------------------------------

-- Given (f a) == r -> a, the type of (.) becomes equivalent to the type of fmap.

-- 22.5 ------------------------------------------------------------------------

newtype Reader r a = Reader {runReader :: r -> a}

rp1 = R.reader (+ 1)

rr1 = R.runReader rp1

rv1 = rr1 4 -- 4+1=5

rt2 :: (Num a) => R.Reader a a
rt2 = R.reader (* 2)

-- I really, really want to be able to do this:
-- rDeep = rp1 + rt2
-- rDeep 6 = (6+1) + (6*2)
-- But Reader doesn't have a Num instance

(+++) :: (Num a) => R.Reader b a -> R.Reader b a -> R.Reader b a
(+++) r1 r2 = (+) <$> r1 <*> r2

rDeep = rp1 +++ rt2

-- Exercise: Ask
ask :: R.Reader a a
ask = R.reader id

-- 22.6: Applicative -----------------------------------------------------------

ff = pure 4 :: String -> Int

-- NOTE: pay attention to the type of <*>.
-- (<*>) :: f (a -> b) -> f a -> f b
-- In this context, I'm reading (f a) as "a function that returns a."
-- Now consider the case where b = x -> y -> z -> etc...
-- This is just currying.
-- Therefore, we can use the function applicative on functions with arbitrary numbers of arguments.
-- Right?

bigFun :: (Fractional a) => a -> a -> a -> a -> a
bigFun i j k l = (i - 2 * j) * (k / l)

pretendThisDoesSomething :: (Num a) => a -> a
pretendThisDoesSomething = (+ 1)

bf1 = bigFun <$> pretendThisDoesSomething -- bf1 w x y z = bigFun (w+1) x y z

bf2 = bf1 <*> pretendThisDoesSomething -- bf2 x y z = bigFun (x+1) (x+1) y z

bf3 = bf2 <*> pretendThisDoesSomething -- bf3 y z = bigFun (y+1) (y+1) (y+1) z

bf4 = bf3 <*> pretendThisDoesSomething -- bf4 z = bigFun (z+1) (z+1) (z+1) (z+1)

-- ASSET equivalent
args1 = id

fourArgsInOne = bigFun <$> args1 <*> args1 <*> args1 <*> args1

-- example

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Dog = Dog {dogsName :: DogName, dogsAddress :: Address} deriving (Eq, Show)

data Person = Person {humanName :: HumanName, dogName :: DogName, address :: Address}
  deriving (Eq, Show)

-- Note that this doesn't actually use the Reader type, it just relies on typeclasses of (->)
getPersonsDog :: Person -> Dog
getPersonsDog = Dog <$> dogName <*> address

-- Exercise

-- 1:
myLiftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y -- Is this what they were looking for? Seems too easy.

-- 2:
-- I'm guessing there have been changes since the book was written.
-- There is no data constructor "Reader" because Reader is kind of a sub-case of ReaderT.
asks :: (r -> a) -> R.Reader r a
asks = R.reader

-- 3:
instance Applicative (Reader r) where
  pure = Reader . const

  (<*>) (Reader rab) (Reader ra) = Reader $ \r -> rab r (ra r)

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

-- 22.7: Monad of Functions ----------------------------------------------------

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+ 1) r

bar :: (Foldable f) => t -> f a -> (t, Int)
bar r t = (r, length t)

-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-- For functions, read arguments as:
-- 1: A function from x to a
-- 2: A function from a to x to b
-- 3: A function from x to b
-- Function binding looks like this: myBind x2a twoArg = \x -> twoArg (x2a x) x
-- It's specifically for the case where the first argument to a function goes through another function, but the second argument can be forwarded directly.
-- Well, but maybe it's not as specific as I thought.
-- Consider the conversation about (<*>).
-- Here, b could also itself be a function.
-- So what does it look like to chain these together?

bb1 = pretendThisDoesSomething >>= bigFun -- bb1 x y z = bigFun (x+1) x y z

bb2 = pretendThisDoesSomething >>= bb1 -- bb2 x y = bigFun (x+1+1) (x+1) x y

bb3 = pretendThisDoesSomething >>= bb2 -- bb3 x = bigFun (x+1+1+1) (x+1+1) (x+1) x

-- Exercise: Reader Monad

instance Monad (Reader r) where
  return = pure

  -- This was very tricky.
  -- It's more complicated than the instance for (->) because you have to pull the function out of the Reader constructor.
  -- That's where runReader comes in, and you have to do it at the right time.
  -- First, you apply the first argument through the r2a function which produces a Reader b.
  -- (a -> Reader r b) ... Reader r b ... (r -> b) ... b
  -- Then, you pull the function out of the reader and apply r sort of as the second argument.
  (>>=) (Reader r2a) a2r2b = Reader $ \r -> (runReader $ a2r2b (r2a r)) r

getPersonsDogM :: Reader Person Dog
getPersonsDogM = do
  dName <- Reader dogName
  addr <- Reader address
  return $ Dog dName addr

pr = Person (HumanName "Shreve") (DogName "Steve") (Address "Florida")

dg = runReader getPersonsDogM pr

-- 22.8: Boring Reader Monad ---------------------------------------------------

-- Page 865 has an implementation of Monad ((->) r) in terms of applicative

-- 22.9 ------------------------------------------------------------------------

-- Think of this "adding an initial layer."
-- The input type used to be r, but now it's q.
-- q passes through function f to become an r before the rest of the reader is evaluated.
withReaderT' :: (q -> r) -> Reader r a -> Reader q a
withReaderT' f r = Reader $ runReader r . f

-- 22.10: ReaderT --------------------------------------------------------------

-- More common, but is a monad transformer, which we won't get to until Ch26.

-- 22.11: Exercises ------------------------------------------------------------

x = [1, 2, 3]

y = [4, 5, 6]

z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' = flip lookup $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: (Num c) => (c, c) -> c
summed (x, y) = x + y

bolt :: Integer -> Bool
bolt x = (3 < x) && (x < 8)

exMain :: IO ()
exMain = do
  print $ sequenceA [Just 3, Just 2, Just 1]

  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]

  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z

  -- Apply this list of functions to a single input
  -- From: list of functions that return y
  -- To: function that returns list of y
  print $ sequenceA [(> 3), (< 8), even] 7

sequA :: (Integral a) => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

officialExMain :: IO ()
officialExMain = do
  -- 1
  print $ fold (BoolConj <$> sequA 8) -- BoolConj is a Monoid implementing (&&). BoolDisj implements (||)

  -- 2
  print $ sequA $ fromMaybe 0 s'

  -- 3
  print $ bolt $ fromMaybe 0 ys

-- URL Shortener
-- I skipped this the first time, and I'm gonna skip it again.

-- 22.12: Definition -----------------------------------------------------------

-- Monad Transformer is monad to monad
