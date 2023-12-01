module Main where

import Day01 (cli)

main :: IO ()
main = do
  putStrLn "Which Day?"
  usrDay <- getLine
  case (read usrDay) :: Int of
    1 -> Day01.cli
