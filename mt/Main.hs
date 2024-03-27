-- Learning parallelism: https://wiki.haskell.org/Parallel/Reading

module Main where

import MT.HFM

-- NOTE: run as "stack run -- mt +RTS -N666"
main :: IO ()
main = mainNaive
