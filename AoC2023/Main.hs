module Main where

import Day01 (cli)
import Day02 (cli)

main :: IO ()
main = do
  putStrLn "Which Day?"
  usrDay <- getLine
  case (read usrDay) :: Int of
    1 -> Day01.cli
    2 -> Day02.cli
