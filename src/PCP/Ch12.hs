module PCP.Ch12 where

import Control.Concurrent
import Control.Concurrent.STM
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

-- Utils -----------------------------------------------------------------------

sendStr :: Socket -> String -> IO ()
sendStr sk = sendAll sk <$> (encodeUtf8 . pack)

initSocket :: IO Socket
initSocket = do
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
  -- give it back
  return sock

-- Server 0 --------------------------------------------------------------------

-- This example uses Network < 3, but Network has been >= 3 since 2018...
-- Prepare for a lot of refactoring.

-- Single-client service
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
        sendStr s "ta ta for now\n"
      else do
        sendStr s (show (2 * (read line :: Integer)) ++ "\n")
        -- Recurse to recieve more
        talk s

server0 :: IO ()
server0 = withSocketsDo $ do
  sock <- initSocket

  -- Handle connections
  forever $ do
    (conn, addr) <- accept sock
    printf "Accepted connection from %s\n" (show addr)
    -- Create dedicated thread
    forkFinally (talk conn) (\_ -> close conn)

-- Server 3: Broadcast Chan ----------------------------------------------------

-- I _sorta_ want to implement this myself, but not really.
-- Especially because option 4 uses TVars instead of MVars, which Marlow himself says are simpler.
-- I already sorta figured that was true, which is why my argus prototypes use TVars

-- Server 4: STM and TVars -----------------------------------------------------

talk4 :: Socket -> TVar Int -> IO ()
talk4 s f = do
  msg <- recv s 1024
  unless (BS.null msg) $ do
    let msgStr = unpack $ decodeUtf8 msg
    let line = head $ lines msgStr
    if line == "end"
      then sendStr s "ta ta for now\n"
      else
        if head line == '*'
          then do
            let newFac = read $ tail line :: Int
            atomically $ writeTVar f newFac
            sendStr s ("Now multiplying by " ++ show newFac ++ "\n")
            talk4 s f
          else do
            let v = read line :: Int
            ff <- readTVarIO f
            sendStr s (show (ff * v) ++ "\n")
            talk4 s f

server4 :: IO ()
server4 = withSocketsDo $ do
  sock <- initSocket
  fac <- newTVarIO (2 :: Int)

  forever $ do
    (conn, addr) <- accept sock
    printf "Accepted connection from %s\n" (show addr)
    forkFinally (talk4 conn fac) (\_ -> close conn)

-- Woooo! Did it by my's self.
-- Buuut... It does seem to take a while to release the port after I Ctrl+C it...
