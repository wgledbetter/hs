-- State

module HB.Ch23 where

import Control.Applicative (liftA2, liftA3)
import Control.Monad (replicateM)
import qualified Control.Monad.Trans.State as S
import System.Random

-- 23.2: What it is ------------------------------------------------------------

-- State type should be used for values that change per step

-- 23.3: Random Numbers --------------------------------------------------------

sg = mkStdGen 0

-- NOTE: The book says to use "next" instead of random, but "next" is deprecated.
-- Relevant issue: https://github.com/haskell/random/issues/114
-- Relevant blog posts:
-- - https://alexey.kuleshevi.ch/blog/2021/01/29/random-interface/#heading-performance
-- - https://alexey.kuleshevi.ch/blog/2019/12/21/random-benchmarks/
r1 :: (Int, StdGen)
r1 = random sg

r2 :: (Double, StdGen)
r2 = random sg

r3 :: [Double]
r3 = map fst $ take 20 $ map (random . mkStdGen) [0 ..]

r4 :: [Double]
r4 = map fst $ take 20 $ map (randomR (-1, 1) . mkStdGen) [0 ..]

-- 23.4: The State Newtype -----------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}

-- 23.5: Throw Down ------------------------------------------------------------

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show, Ord)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "die value out of bounds: " ++ show x

rollDie :: S.State StdGen Die
rollDie = S.state $ do
  -- S.state is analogous to R.reader because S.State is actually an alias for S.StateT.
  (n, s) <- randomR (1, 6) -- :: s -> (n, s)
  return (intToDie n, s)

rollDie' :: S.State StdGen Die
rollDie' = intToDie <$> S.state (randomR (1, 6))

dieRoll = S.evalState rollDie sg

dieRollWithState = S.runState rollDie' sg

rollDieX3 :: S.State StdGen (Die, Die, Die)
rollDieX3 = liftA3 (,,) rollDie rollDie rollDie

-- Note that even though the non-state value comes first in the tuple, it is second in the type definition.
-- This is so the (* -> *) typeclasses can operate on the emitted value rather than the state.
-- In rollDieX3, for example, we're lifting the multi-tuple operator over the Die part of "State StdGen Die".

nDie :: Int -> S.State StdGen [Die]
nDie n = replicateM n rollDie'

fiveRolls = S.evalState (nDie 5) sg

rollsToSum :: Int -> StdGen -> Int
rollsToSum total gn = go 0 0 gn
  where
    go sum rolls gen
      | sum >= total = rolls
      | otherwise =
          let (thisRoll, nGen) = randomR (1, 6) gen
           in go (sum + thisRoll) (rolls + 1) nGen

average :: (Fractional a) => [a] -> a
average = liftA2 (/) sum (fromIntegral . length)

avgRollsToSum :: Int -> Int -> Int -> Double
avgRollsToSum genStart nSamples sum = average $ map fromIntegral rolls
  where
    rolls = take nSamples $ map (rollsToSum sum . mkStdGen) [genStart ..]

-- Exercise 2:
whichRollsToSum :: Int -> StdGen -> (Int, [Die])
whichRollsToSum total gn = go 0 0 gn []
  where
    go sum rolls gen history
      | sum >= total = (rolls, history)
      | otherwise =
          let (thisRoll, nGen) = randomR (1, 6) gen
           in go (sum + thisRoll) (rolls + 1) nGen (intToDie thisRoll : history)

-- 23.6: DIY -------------------------------------------------------------------

instance Functor (State s) where
  fmap f (State s) = State (\ss -> let (v, s') = s ss in (f v, s'))

instance Applicative (State s) where
  pure x = State (\s -> (x, s))

  -- NOTE: This is nearly identical to the official implementation, except they use do-notation
  (<*>) (State f) (State x) =
    State
      ( \s ->
          let (f1, s') = f s -- Get the function from the first argument
           in let (v, s'') = x s' -- Get the value from the second argument, using state from above
               in (f1 v, s'') -- "Apply" function to value
      )

instance Monad (State s) where
  return = pure

  -- Remember that the monad for (->) already exists.
  (>>=) (State f) g =
    State
      ( \s ->
          let (x, s') = f s -- Evaluate function in first argument
              State gg = g x -- Evaluate second argument with first value to get a new state function
           in gg s' -- Evaluate the new state function with the state generated during first argument evaluation
      )

-- 23.7 ------------------------------------------------------------------------

isDivBy :: Integer -> Integer -> Bool
isDivBy denom numer = numer `mod` denom == 0

int2fb :: Integer -> String
int2fb i
  | isDivBy 15 i = "FizzBuzz"
  | isDivBy 5 i = "Buzz"
  | isDivBy 3 i = "Fizz"
  | otherwise = show i

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = map int2fb [from .. to]

-- Takes an integer and returns an operation that fizzbuzzes that integer onto the list of strings.
fizzbuzzSt :: Integer -> S.State [String] ()
fizzbuzzSt i = S.state (\s -> ((), int2fb i : s))

fizzbuzzFromToSt :: Integer -> Integer -> [String]
fizzbuzzFromToSt from to = S.execState fbState []
  where
    fbState = traverse fizzbuzzSt $ enumFromThenTo to (to - 1) from

-- NOTE: mapM and traverse seem to be the same thing.

-- 23.8: Exercises -------------------------------------------------------------

-- 1:
-- Kind of a transition from "state space" into "value space."
get :: State s s
get = State (\s -> (s, s))

-- 2:
put :: s -> State s ()
put s = State (const ((), s))

-- 3:
exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s

-- 4:
eval :: State s a -> s -> a
eval (State sa) s = fst $ sa s

-- 5:
modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))

modP1 = modify (+ 1)

ex5 = runState (modP1 >> modP1 >> modP1) 0

-- I keep feeling like there should be another argument to these functions.
-- My intuition is correct, it's just that that argument is wrapped inside the State type
