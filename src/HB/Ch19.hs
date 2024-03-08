-- Monoids, Functors, Applicatives, and Monads in real code

module HB.Ch19 where

import Control.Applicative
import qualified Data.Map as M

-- Monoid of functions ---------------------------------------------------------

newtype Fun a b = Fun (a -> b)

call :: Fun a b -> a -> b
call (Fun f) = f

instance (Semigroup b) => Semigroup (Fun a b) where
  (Fun f) <> (Fun g) = Fun (\x -> (f x) <> (g x))

instance (Monoid b) => Monoid (Fun a b) where
  mempty = Fun (\_ -> mempty)
  mappend = (<>)

-- Monoid of Data.Map ----------------------------------------------------------

fm = M.fromList [('a', 1)]

gm = M.fromList [('b', 2)]

hm = M.fromList [('a', 3)]

-- left-most instance of key is preferred
-- This functionality is used in XMonad to overwrite default configurations with user-defined preferences.

mex1 = fm <> hm

mex2 = hm <> fm

-- Question:
-- In these (<$), (*>), and (>>) "sequencing"-type functions, the explanation is "do x, discard it, then do y," but if things are lazy evaluated and discarded, how do you know they will actually be triggered?
-- Presumably, you'd have to use the result of x somehow, but in the GHC implementations I've seen, these functions reach into `const`, whose implementation is: `const x _ = x`, clearly just dropping the second value on the floor.

works1 = [1, 3] <* [undefined] -- See?

works2 = [1, 2, 3] <$ [undefined]

works3 = [undefined] >> [7, 8, 9]

works4 = [undefined] *> [4, 5, 6]

-- One of the examples in the Functor section used `many`, and that led me to `Alternative`:
-- https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus

digit :: Int -> String -> Maybe Int
digit i (c : _)
  | i < 0 || 9 < i = Nothing
  | otherwise = if [c] == show i then Just i else Nothing

-- The two attempted parses of 0 and 1 can be run in parallel, and their results combined for the output
binChar :: String -> Maybe Int
binChar s = digit 0 s <|> digit 1 s

-- This whole pattern where you run a bunch of stuff and essentially filter out the failures is cool and all, except what if you care about the inputs that succeeded?
-- In the above example, we know which parser's result is returned because it's embedded in the return value, but, I dunno, suppose you're doing some kind of monte carlo analysis.
-- These kind of toy problems would just tell you that _something_ worked.
-- There would be additional structure required to tell you what exactly it was that worked.
-- I ran into this problem in AoC.
-- Something like "does this row have some pattern" and it'd just say "yup" and not tell me where it was unless I did some kind of feedthrough.

-- replicateM is a nice function.
replicate'em :: (Applicative m) => Int -> m a -> m [a]
replicate'em 0 _ = pure []
replicate'em n a = (:) <$> a <*> replicate'em (n - 1) a

-- replicate'em n a = [x : rest | x <- a, rest <- replicate'em (n - 1) a]

-- replicate'em n a = do
--   x <- a
--   rest <- replicate'em (n - 1) a
--   pure (x : rest)
