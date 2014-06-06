-- | Test our chat server.
module Main (main) where

import Test.Hspec

import Chat

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Char
import Data.Functor
import Data.Maybe
import Network
import System.IO
import System.Posix.Env

port :: Int
port = 8000

main :: IO ()
main = withSocketsDo $ do
  setEnv "CHAT_SERVER_PORT" (show port) True
  void $ async startServer
  threadDelay 10000
  spec
  return ()

newClient :: IO Handle
newClient = do
  conn <- connectTo "localhost" (PortNumber $ toEnum port)
  hSetEncoding conn utf8
  hSetBuffering conn LineBuffering
  return conn

scopedClient :: (Handle -> IO a) -> IO a
scopedClient m = bracket newClient hClose m

testConnect :: IO Int
testConnect = bracket newClient hClose $ \conn -> do
  msg <- getInput conn
  return $ parseWelcomeMsg msg

checkWelcomeMsg :: Handle -> Int -> IO ()
checkWelcomeMsg h cid = (parseWelcomeMsg <$> getInput h) `shouldReturn` cid

parseWelcomeMsg :: String -> Int
parseWelcomeMsg msg | [cid, "has", "joined"] <- parts = read cid
                    | otherwise = error "unexpected welcome messagge"
  where parts = words $ map toLower msg

parseLeaveMsg :: String -> Int
parseLeaveMsg msg | [cid, "has", "left"] <- parts = read cid
                  | otherwise = error "unexpected exit messagge"
  where parts = words $ map toLower msg

checkForInput :: Handle -> IO (Maybe String)
checkForInput h = do
  a <- async $ hGetLine h
  threadDelay 10000
  cancel a
  r <- poll a
  case r of
    Nothing        -> return Nothing
    Just (Left e)  -> throw e
    Just (Right s) -> return $ Just s

consumeInput :: Handle -> IO ()
consumeInput = void . checkForInput

shouldBeNoInput :: Handle -> IO ()
shouldBeNoInput h = checkForInput h `shouldReturn` Nothing

getInput :: Handle -> IO String
getInput h = fromJust <$> checkForInput h

spec :: IO ()
spec = hspec $ describe "Testing Lab 2" $ do
  describe "connecting" $ do
    it "as new client" $ do
      testConnect `shouldReturn` 1

    it "should increment the client id" $ do
      testConnect `shouldReturn` 2
      testConnect `shouldReturn` 3
      testConnect `shouldReturn` 4

    it "broadcasts welcome message" $ scopedClient $ \client -> do
      checkWelcomeMsg client 5
      testConnect `shouldReturn` 6
      checkWelcomeMsg client 6

  describe "disconnect" $ do
    it "server sends exit msg" $ scopedClient $ \client -> do
      (parseWelcomeMsg <$> getInput client) `shouldReturn` 7
      testConnect `shouldReturn` 8
      (parseWelcomeMsg <$> getInput client) `shouldReturn` 8
      (parseLeaveMsg <$> getInput client) `shouldReturn` 8
      testConnect `shouldReturn` 9
      (parseWelcomeMsg <$> getInput client) `shouldReturn` 9
      (parseLeaveMsg <$> getInput client) `shouldReturn` 9

  describe "sending messages" $ do
    it "doesn't have own messages echoed back" $ scopedClient $ \client -> do
        void $ parseWelcomeMsg <$> getInput client
        hPutStrLn client "don't echo back"
        shouldBeNoInput client
        replicateM_ 100 $ hPutStrLn client "don't echo back again"
        shouldBeNoInput client
        shouldBeNoInput client

    it "can send messages to each other" $
      scopedClient $ \c1 -> scopedClient $ \c2 -> do
        c1id <- parseWelcomeMsg <$> getInput c1
        c2id <- parseWelcomeMsg <$> getInput c1
        (parseWelcomeMsg <$> getInput c2) `shouldReturn` c2id

        hPutStrLn c1 "bangers and mash"
        shouldBeNoInput c1
        getInput c2 `shouldReturn` (show c1id ++ ": bangers and mash")

        hPutStrLn c2 "babar the elephant"
        shouldBeNoInput c2
        getInput c1 `shouldReturn` (show c2id ++ ": babar the elephant")

    it "supports clients coming and going" $
      scopedClient $ \c1 -> scopedClient $ \c2 -> do
        c1id <- parseWelcomeMsg <$> getInput c1
        c2id <- parseWelcomeMsg <$> getInput c1
        (parseWelcomeMsg <$> getInput c2) `shouldReturn` c2id

        hPutStrLn c1 "bangers and mash"
        shouldBeNoInput c1
        getInput c2 `shouldReturn` (show c1id ++ ": bangers and mash")

        shouldBeNoInput c1
        shouldBeNoInput c2

        c3id <- scopedClient $ \c3 -> do
          c3id <- parseWelcomeMsg <$> getInput c1
          (parseWelcomeMsg <$> getInput c2) `shouldReturn` c3id
          (parseWelcomeMsg <$> getInput c3) `shouldReturn` c3id

          shouldBeNoInput c1
          shouldBeNoInput c2
          shouldBeNoInput c3

          hPutStrLn c2 "babar the elephant"
          shouldBeNoInput c2
          getInput c1 `shouldReturn` (show c2id ++ ": babar the elephant")
          getInput c3 `shouldReturn` (show c2id ++ ": babar the elephant")

          hPutStrLn c3 "banana man was great"
          shouldBeNoInput c3
          getInput c1 `shouldReturn` (show c3id ++ ": banana man was great")
          getInput c2 `shouldReturn` (show c3id ++ ": banana man was great")
          return c3id
        --
        (parseLeaveMsg <$> getInput c1) `shouldReturn` c3id
        (parseLeaveMsg <$> getInput c2) `shouldReturn` c3id
        shouldBeNoInput c1
        shouldBeNoInput c2

        hPutStrLn c1 "rockos modern life"
        shouldBeNoInput c1
        getInput c2 `shouldReturn` (show c1id ++ ": rockos modern life")
        shouldBeNoInput c2

  describe "bonus" $ do
    describe "rooms" $ do
      it "they work" $
        scopedClient $ \c1 -> scopedClient $ \c2 -> do
          c1id <- parseWelcomeMsg <$> getInput c1
          c2id <- parseWelcomeMsg <$> getInput c1
          (parseWelcomeMsg <$> getInput c2) `shouldReturn` c2id

          hPutStrLn c1 "bangers and mash"
          shouldBeNoInput c1
          getInput c2 `shouldReturn` (show c1id ++ ": bangers and mash")

          -- not specified if leaving message should be sent...
          hPutStrLn c1 "/join rugrats"
          consumeInput c1
          consumeInput c2

          hPutStrLn c1 "loud noises"
          shouldBeNoInput c1
          shouldBeNoInput c2

          -- not specified if joining message should be sent...
          hPutStrLn c2 "/join rugrats"
          consumeInput c1
          consumeInput c2

          hPutStrLn c1 "adventure time"
          shouldBeNoInput c1
          getInput c2 `shouldReturn` (show c1id ++ ": adventure time")

          c3id <- scopedClient $ \c3 -> do
            c3id <- parseWelcomeMsg <$> getInput c3
            -- unspecified if c1, c2 should receive welcome messages
            consumeInput c1
            consumeInput c2

            hPutStrLn c3 "jake the dog"
            shouldBeNoInput c3
            shouldBeNoInput c1
            shouldBeNoInput c2

            hPutStrLn c1 "/join finn"
            consumeInput c1
            hPutStrLn c3 "/join finn"
            consumeInput c1
            consumeInput c3

            hPutStrLn c1 "ice king"
            shouldBeNoInput c1
            shouldBeNoInput c2
            getInput c3 `shouldReturn` (show c1id ++ ": ice king")
            return c3id
          --
          (parseLeaveMsg <$> getInput c1) `shouldReturn` c3id

    describe "private messages" $ do
      it "they work" $
        scopedClient $ \c1 ->
        scopedClient $ \c2 ->
        scopedClient $ \c3 -> do

        c1id <- parseWelcomeMsg <$> getInput c1
        c2id <- parseWelcomeMsg <$> getInput c1
        c3id <- parseWelcomeMsg <$> getInput c1
        (parseWelcomeMsg <$> getInput c2) `shouldReturn` c2id
        (parseWelcomeMsg <$> getInput c2) `shouldReturn` c3id
        (parseWelcomeMsg <$> getInput c3) `shouldReturn` c3id

        hPutStrLn c1 "bangers and mash"
        shouldBeNoInput c1
        getInput c2 `shouldReturn` (show c1id ++ ": bangers and mash")
        getInput c3 `shouldReturn` (show c1id ++ ": bangers and mash")

        hPutStrLn c1 $ "/msg " ++ show c2id ++ " babar the elephant"
        shouldBeNoInput c1
        shouldBeNoInput c3
        getInput c2 `shouldReturn` (show c1id ++ ": babar the elephant")

