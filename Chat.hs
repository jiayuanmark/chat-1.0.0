-- | CS240h Lab 2 Chat Server
module Chat (startServer) where

import Network
import Data.ByteString (ByteString)
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import System.IO
import System.Environment (getEnv)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Fix (fix)


type UserName = Int
type Message = (UserName, ByteString)


-- | Get chat server port.
readPort :: IO Int
readPort = liftM read $ getEnv "CHAT_SERVER_PORT"


-- | Chat server entry point.
startServer :: IO ()
startServer = do
  chan <- newBroadcastTChanIO
  port <- readPort
  bracket (listenOn . PortNumber $ fromIntegral port) (sClose) $ \s -> do
  	putStrLn $ "Chat server starts listening on port " ++ show port
  	serverLoop s chan 1


-- | Main server loop.
serverLoop :: Socket -> TChan Message -> UserName -> IO ()
serverLoop s chan user = do
  bracket (accept s) (\(h, _, _) -> hClose h) $ 
    \(h, host, port) -> do
      putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
      _ <- forkIO $ serveRequest h chan user
      serverLoop s chan $! user+1


encode :: String -> ByteString
encode = encodeUtf8 . pack

decode :: ByteString -> String
decode = unpack . decodeUtf8


-- | Serve request.
serveRequest :: Handle -> TChan Message -> UserName -> IO ()
serveRequest hdl chan user = do
  let broadcast msg = atomically $ writeTChan chan (user, encode msg)
  hPutStrLn hdl $ show user ++ " has joined"
  broadcast $ show user ++ " has joined"
  chan' <- atomically $ dupTChan chan
  reader <- forkIO $ fix $ \loop -> do
  	(usr, line) <- atomically $ readTChan chan'
  	when (user /= usr) $ hPutStrLn hdl (decode line)
  	loop
  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
  	line <- liftM init (hGetLine hdl)
  	broadcast $ show user ++ ": " ++ line
  	loop
  killThread reader
  broadcast $ show user ++ " has left."
  hClose hdl

