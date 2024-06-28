module Main where

import qualified PCP.Ch12

-- PCP Dispatch ----------------------------------------------------------------

pcpDispatch :: IO ()
pcpDispatch = do
  putStrLn "I've only got Ch12s servers and clients. Give me server# or client#."
  sc <- getLine
  case sc of
    "server0" -> PCP.Ch12.server0
    "client0" -> PCP.Ch12.client0

-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Which Book?"
  book <- getLine
  case book of
    "PCP" -> pcpDispatch
