module Main where

import qualified PCP.Ch12
import Text.Printf

-- PCP Dispatch ----------------------------------------------------------------

pcpDispatch :: IO ()
pcpDispatch = do
  putStrLn "I've only got Ch12s servers. Give me server#."
  sc <- getLine
  case sc of
    "server0" -> PCP.Ch12.server0
    "server4" -> PCP.Ch12.server4
    _ -> printf "Ain't got a %s\n" sc

-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Which Book?"
  book <- getLine
  case book of
    "PCP" -> pcpDispatch
    _ -> printf "No book named %s\n" book
