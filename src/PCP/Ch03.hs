module PCP.Ch03 where

import Control.Parallel.Strategies (NFData, Strategy, rdeepseq, rpar, rparWith, runEval, using)
import Memo (fixedFib, memoizedFib, slowFib)
import PCP.Ch02 (MyEval)

-- Evaluation Strategies -------------------------------------------------------

type MyStrat a = a -> MyEval a

-- rpar and rseq from Ch02 are Strategies
-- Use them to build more complex strategies

parPair' :: Strategy (a, b)
parPair' (a, b) = do
  a' <- rpar a
  b' <- rpar b
  return (a', b')

using' :: a -> Strategy a -> a
x `using'` s = runEval (s x)

-- Remember, the whole reason we can apply a strategy to a value is because
--   Haskell is lazy and hasn't actually computed the value yet.

-- Tangent: What happens if I parallelize evaluation of a memoized function?

ppNoMem x y = (slowFib x, slowFib y) `using` parPair'

ppMem x y = (fixedFib x, fixedFib y) `using` parPair'

-- Answer: ppNoMem is waaaaaaay faster than ppMem when cores > 1
-- In fact, ppNoMem slows down dramatically as you add more cores.

-- Parameterized Strategies ----------------------------------------------------

evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa sb (a, b) = do
  a' <- sa a
  b' <- sb b
  return (a', b')

parPair'' = evalPair rpar rpar -- Only evaluates to WHNF?

parPair''' :: Strategy a -> Strategy b -> Strategy (a, b)
parPair''' sa sb = evalPair (rparWith sa) (rparWith sb)

nfPair :: (NFData a, NFData b) => Strategy (a, b)
nfPair = parPair''' rdeepseq rdeepseq

-- Doesn't do anything. The value is still readable and computable, but we haven't given it any directives regarding its' method of computation.
rdont :: Strategy a
rdont = return

-- Parallel lists --------------------------------------------------------------

-- Use a strategy for every element in a list
parList' :: Strategy a -> Strategy [a]
parList' _ [] = return []
parList' s (x : xs) = do
  x' <- s x -- Apply the strat
  xs' <- parList' s xs -- Recurse
  return (x' : xs')

-- Apply map as usual, but then say to evaluate it with sb. Again, possible because of Haskell's laziness.
-- The book hard-codes "sb" to rseq. Is there a reason for that? Maybe for infinite lists?
-- No, I checked infinite lists (which broke) and timings, and the use of rseq does in fact mean there's no parallelism. Runs in the same time as standard map.
-- See mt/Main.hs
-- Oh, something is different about the parList in the actual library.
-- My parList' above doesn't create any sparks, but the official Control.Parallel.Strategies.parList _does_ create sparks, enabling parallelism even with "rseq".
-- Ah, yes, it's because [parTraversable strat = evalTraversable (rparWith strat)].
-- Library functions still don't like infinite lists though.
parMap' :: (a -> b) -> [a] -> Strategy b -> [b]
parMap' f xs sb = (map f xs) `using` (parList' sb)

-- Example: K-Means ------------------------------------------------------------
-- https://open.spotify.com/track/47gbvFPyt5fSSRMrq4H5i4?si=7a24cd6eb3c840eb
-- Lloyd's algorithm

-- Instead of instantiating groups of points per cluster, it instead builds up the PointSum value for each cluster.
-- This avoids constructing intermediate structures and then iterating over them.
-- So, instead of something like splitting a list into 3 lists and then summing each list, you just run the filter over the original list and apply the sum operation incrementally.

sumOddsEvens :: (Integral a) => [a] -> (a, a)
sumOddsEvens l = (sum o, sum e)
  where
    o = filter odd l
    e = filter even l

sumOddsEvens' :: (Integral a) => [a] -> (a, a)
sumOddsEvens' = foldr (\n (o, e) -> if even n then (o, e + n) else (o + n, e)) (0, 0)

-- I just benched these and the one with intermediate data structures is nearly 10x faster...
-- Maybe it's because its being used on such a simple data type. Whatever.
-- This is the story of my life with Haskell:
-- "Oh, let me test this abstraction that's supposed to make things faster."
-- >10x pessimization
