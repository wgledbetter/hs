module LC where

import HB.Ch09 (myMaximumBy)

-- Largest palindromic substring -----------------------------------------------

allSubstrs :: String -> [String]
allSubstrs [] = []
allSubstrs s = [take x s | x <- enumFromTo 1 (length s)] ++ allSubstrs rest
  where
    (first : rest) = s

isPalin :: String -> Bool
isPalin s = s == reverse s

largestSubPalin :: String -> String
largestSubPalin =
  myMaximumBy (\x y -> compare (length x) (length y))
    . filter isPalin
    . allSubstrs
