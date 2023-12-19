module Day15 where

import Data.Char (ord)
import HB.Ch09 (splitAndDrop)
import System.IO

-- Puz 1 -----------------------------------------------------------------------

testInput = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

testOutput = 1320

hashCore :: Int -> Char -> Int
hashCore start c = rem (17 * (start + ord c)) 256

hash :: String -> Int
hash  = foldl hashCore 0

sol :: String -> Int
sol = sum . map hash . splitAndDrop ',' . init -- drop trailing newline

-- IO --------------------------------------------------------------------------

cli :: IO ()
cli = do
  putStrLn "Welcome to Day 15!"
  putStrLn "Which puzzle would you like to solve (1)?"
  puzNum <- getLine
  fHandle <- openFile "AoC2023/input/Day15.txt" ReadMode
  raw <- hGetContents fHandle
  case read puzNum :: Int of
    1 -> (print . sol) raw
    _ -> print "Invalid puzzle."
