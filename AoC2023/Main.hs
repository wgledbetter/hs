module Main where

import Day01 (cli)
import Day02 (cli)
import Day03 (cli)
import Day04 (cli)
import Day05 (cli)

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
    _ -> print "Invalid Day."
