module Main where

import Day01 (cli)
import Day02 (cli)
import Day03 (cli)
import Day04 (cli)
import Day05 (cli)
import Day06 (cli)
import Day07 (cli)
import Day08 (cli)
import Day09 (cli)
import Day10 (cli)
import Day11 (cli)
import Day12 (cli)

main :: IO ()
main = do
  putStrLn "Which Day?"
  usrDay <- getLine
  case (read usrDay) :: Int of
    1 -> Day01.cli
    2 -> Day02.cli
    3 -> Day03.cli
    4 -> Day04.cli
    5 -> Day05.cli
    6 -> Day06.cli
    7 -> Day07.cli
    8 -> Day08.cli
    9 -> Day09.cli
    10 -> Day10.cli
    11 -> Day11.cli
    12 -> Day12.cli
    _ -> print "Invalid Day."
