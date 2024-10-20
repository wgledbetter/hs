module Math.Opt where

-- Bisection -------------------------------------------------------------------
-- Gotta start somewhere...

bisect :: (Fractional a, Ord a) => a -> (a -> a) -> (a, a) -> a
bisect tol f (lo, hi)
  | hi - lo <= tol = lo
  | sigMid == 0 = mid
  | otherwise = bisect tol f (newLo, newHi)
  where
    mid = (lo + hi) / 2

    sigLo = signum $ f lo
    sigMid = signum $ f mid
    sigHi = signum $ f hi

    newLo = if sigMid == sigLo then mid else lo
    newHi = if sigMid == sigHi then mid else hi
