-- | CS240h Lab 2 Chat Server
module Chat (startServer) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Functor
import Network
import System.Environment (getEnv)
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T


type UserName = Int
type Message = (UserName, T.Text)


-- | Get chat server port.
readPort :: IO Int
readPort = read <$> getEnv "CHAT_SERVER_PORT"


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
      hSetEncoding h utf8
      hSetBuffering h LineBuffering
      void $ forkIO $ serveRequest h chan user
      serverLoop s chan $! user+1


-- | Serve request.
serveRequest :: Handle -> TChan Message -> UserName -> IO ()
serveRequest hdl chan user = do
  T.hPutStrLn hdl $ joinMsg user
  broadcast $ joinMsg user
  ch <- atomically $ dupTChan chan
  void $ forkIO $ reader ch
  writer
  where
    broadcast msg' = atomically $ writeTChan chan (user, msg')

    joinMsg user' = T.pack $ show user' ++ " has joined"

    leftMsg user' = T.pack $ show user' ++ " has left"

    chatMsg user' msg' = T.pack $ show user' ++ ": " ++ msg'

    reader chan' = do
      (usr, line) <- atomically $ readTChan chan'
      when (user /= usr) $ T.hPutStrLn hdl line
      reader chan'
      `catch`
      ((\_-> hClose hdl) :: SomeException -> IO ())

    writer = do
      line <- hGetLine hdl
      broadcast $ chatMsg user line
      writer
      `catch`
      ((\_-> broadcast (leftMsg user) >> hClose hdl) :: SomeException -> IO ())




