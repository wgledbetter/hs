module PCP.Ch12 where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BS
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO
import Text.Printf

-- Settings --------------------------------------------------------------------

port :: Int
port = 44444

-- Server 0 --------------------------------------------------------------------

-- This example uses Network < 3, but Network has been >= 3 since 2018...
-- Prepare for a lot of refactoring.

talk :: Socket -> IO ()
talk s = do
  -- Get ByteString from socket
  msg <- recv s 1024
  unless (BS.null msg) $ do
    -- Decode bytes to characters
    let msgStr = unpack $ decodeUtf8 msg
    -- get characters before newline
    let line = head $ lines msgStr
    if line == "end"
      then -- The absence of recursion in this case causes termination
        sendStr "ta ta for now\n"
      else do
        sendStr (show (2 * (read line :: Integer)) ++ "\n")
        -- Recurse to recieve more
        talk s
  where
    -- Convert String to Text to ByteString and send over the socket
    sendStr = sendAll s <$> (encodeUtf8 . pack)

server0 :: IO ()
server0 = withSocketsDo $ do
  -- Create hints for address search
  let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
  -- Search for address
  addr <- head <$> getAddrInfo (Just hints) Nothing (Just $ show port)
  -- Create socket with some of the address info
  sock <- openSocket addr
  -- Bind the socket to the full address
  bind sock $ addrAddress addr
  -- Start listening
  listen sock 1024
  printf "Listening on %s\n" (show addr)

  -- Handle connections
  forever $ do
    (conn, addr) <- accept sock
    printf "Accepted connection from %s\n" (show addr)
    forkFinally (talk conn) (\_ -> close conn)

-- Client 0 --------------------------------------------------------------------

client0 :: IO ()
client0 = withSocketsDo $ do
  putStrLn "gimme a second"
