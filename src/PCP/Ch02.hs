module PCP.Ch02 where

import Control.Parallel.Strategies (Eval, rpar, rseq, runEval)
import Prelude (IO, Int, Monad, return, seq, undefined, ($), (+))

-- Lazy Evaluation and Weak Head Normal Form -----------------------------------

x = 1 + 2 :: Int

z = seq (x, x + 1) () -- Only evaluates the tuple structure, not the contents. WHNF.

-- The Eval Monad, rpar, and rseq ----------------------------------------------

-- rpar and rseq have type "Strategy a = a -> Eval a"

newtype MyEval a = MyEval {runMyEval :: IO a} -- Important: Eval just holds an IO.

-- Dummy Function
f :: Int -> Int
f = undefined

i :: Int
i = undefined

j :: Int
j = undefined

-- rpar/rpar:
-- Returns immediately. Tuple values are computed "asynchronously" and will "arrive" later.
rprp = runEval $ do
  a <- rpar (f i)
  b <- rpar (f j)
  return (a, b)

-- rpar/rseq:
-- Returns once (f j) has completed calculation. `a` will "arrive asynchronously."
rprs = runEval $ do
  a <- rpar (f i)
  b <- rseq (f j)
  return (a, b)

-- rpar/rseq/rseq:
-- `a` is "dispatched" to run in parallel.
-- `b` is blocking until it finishes, at which time we'll then block until `a` finishes.
-- Then we'll return.
rprsrs = runEval $ do
  a <- rpar (f i)
  b <- rseq (f j)
  rseq a -- Think std::thread().join(), except as an IO monad.
  return (a, b)

-- rpar/rpar/rseq/rseq
-- Effectively the same as rpar/rseq/rseq. Maybe visually nicer
rprprsrs = runEval $ do
  a <- rpar (f i)
  b <- rpar (f j)
  rseq a
  rseq b
  return (a, b)

-- Sudoku Examples -------------------------------------------------------------

-- force :: (NFData a) => a -> a
-- Evaluates WHNF to Normal Form, enabling parallelism over lists.
-- For example, `rpar (map solve xs)` would not traverse the whole list.
-- We have to say `rpar (force (map solve xs))`.
-- Comes from "DeepSeq".
-- Common mistake to not evaluate deeply enough.

-- The "+RTS -s" flag shows statistics. It's got timings, memory, and threading info.
-- We can see how many "sparks" were created to run in parallel.

-- Use GHC "-eventlog" flag to dump info for threadscope

-- Oh, oh, lemme guess:
-- Are we going to map rpar over the list?
-- Yeah, basically. See `parMap :: (a -> b) -> [a] -> Eval [b]` in sudoku3

-- Static partitioning: User chooses X number of parallel jobs.

-- Spark statuses:
-- - converted: turned into parallel jobs?
-- - overflowed: exceeded spark pool size
-- - dud: tried to spark value that already had been computed
-- - GC'd: unused
-- - fizzled: evaluated later by itself (by the primary thread maybe? if there is a primary thread?)

-- Deepseq ---------------------------------------------------------------------

-- Evaluate x to normal form then return x
-- force x = x `deepseq` x
