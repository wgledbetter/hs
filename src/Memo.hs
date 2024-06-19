-- Memoization Experiments

module Memo where

import Data.Function (fix)

-- The Wiki Page ---------------------------------------------------------------

-- Obligatory Fibonacci Example

slowFib :: Int -> Integer
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n - 2) + slowFib (n - 1)

memoizedFib :: Int -> Integer
memoizedFib = (map fib [0 ..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = memoizedFib (n - 2) + memoizedFib (n - 1)

fib :: (Int -> Integer) -> Int -> Integer
fib f 0 = 1
fib f 1 = 1
fib f n = f (n - 2) + f (n - 1)

memoizeInt :: (Int -> a) -> (Int -> a)
memoizeInt fi = (map fi [0 ..] !!)

fixedFib :: Int -> Integer
fixedFib = fix (memoizeInt . fib)
