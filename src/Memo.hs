-- Memoization Experiments

module Memo where

import Data.Function (fix)

-- The Wiki Page ---------------------------------------------------------------

-- Obligatory Fibonacci Example

slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n - 2) + slow_fib (n - 1)

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = memoized_fib (n - 2) + memoized_fib (n - 1)

fib:: (Int->Integer) -> Int -> Integer
fib f 0 = 1
fib f 1 = 1
fib f n = f (n-2) + f (n-1)

memoizeInt :: (Int ->a) -> (Int -> a)
memoizeInt fi = (map fi [0..] !!)

fixed_fib :: Int->Integer
fixed_fib = fix (memoizeInt . fib)
