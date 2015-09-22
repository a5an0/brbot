{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8 (putStrLn)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Foldable (Foldable, traverse_)
import GHC.Generics
import Control.Applicative
import Data.List.Split
import System.Environment
import Wuss
import Control.Concurrent (forkIO)
import Control.Error.Safe (atErr)
import Control.Monad (forever, unless, void, when, liftM)
import Data.Text (Text, pack)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Network.WebSockets.Connection as WS (Connection)

import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data RtmStart = RtmStart
                { ok :: Bool
                , url :: String
                } deriving (Show,Generic)
instance FromJSON RtmStart
instance ToJSON RtmStart

data RtmMessage = RtmMessage
                  { typ :: String
                  , channel :: String
                  , user :: String
                  , text :: String
                  , ts :: String
                  } deriving (Show)
instance FromJSON RtmMessage where
  parseJSON (Object x) = RtmMessage
                         <$> x .: "type"
                         <*> x .: "channel"
                         <*> x .: "user"
                         <*> x .: "text"
                         <*> x .: "ts"
  parseJSON _ = empty

instance ToJSON RtmMessage where
  toJSON message = object
                   [ "type" .= typ message
                   , "channel" .= channel message
                   , "user" .= user message
                   , "text" .= text message
                   , "ts" .= ts message
                   ]

data RtmReplyMsg = RtmReplyMsg
                   { mId :: Int
                   , repTyp :: String
                   , repChannel :: String
                   , repText :: String
                   } deriving (Show)
instance FromJSON RtmReplyMsg where
  parseJSON (Object x) = RtmReplyMsg
                         <$> x .: "id"
                         <*> x .: "type"
                         <*> x .: "channel"
                         <*> x .: "text"
  parseJSON _ = empty

instance ToJSON RtmReplyMsg where
  toJSON message = object
                   [ "id" .= mId message
                   , "type" .= repTyp message
                   , "channel" .= repChannel message
                   , "text" .= repText message
                   ]

someFunc :: IO ()
someFunc = do
  tuple <- wsConnTuple
  case tuple of
    Left e -> putStrLn e
    Right (hostname, path) -> runSecureClient hostname 443 path ws

wsConnTuple :: IO (Either String (String, String))
wsConnTuple = do
  rawUrl <- getWsUrl
  let splitList = splitOn "/" rawUrl
  return $ do
    let err = "malformed url (wsConnTuple): " ++ rawUrl
    hostname <- atErr err splitList 2
    path0    <- atErr err splitList 3
    path1    <- atErr err splitList 4
    return (hostname, "/" ++ path0 ++ "/" ++ path1)

getWsUrl :: IO String
getWsUrl = do
  decodedJson <- decode <$> rtmJson
  return $ case decodedJson of
    Just rtm -> url rtm
    Nothing  -> ""

rtmJson :: IO L.ByteString
rtmJson =  do
  urlString <- ("https://slack.com/api/rtm.start?token=" ++) <$> getEnv "BRBOT_TOKEN"
  case parseUrl urlString of
    Nothing -> return L.empty
    Just req -> withManager $ \manager -> do
      res <- httpLbs req manager
      liftIO $ return $ responseBody res


buildReply :: String -- channel
              -> String -- sending user
              -> String -- user that's not here
              -> L.ByteString
buildReply chan reqUser awayUser = encode $ RtmReplyMsg 1 "message" chan ("<@" ++ reqUser ++ ">: <@" ++ awayUser ++"> is away right now.")

ws :: ClientApp ()
ws connection = do
    putStrLn "Connected!"

    void . forkIO . forever $ do
        msg <- receiveData connection :: IO L.ByteString
        decodedMsg <- case (decode msg :: Maybe RtmMessage) of
          Just rtmMsg -> return rtmMsg
          Nothing -> return $ RtmMessage "" "" "" "" ""
        let uidList = liftM (fmap uid) allAways
        let mentionedUsers = filter (\x -> ("<@" ++ x ++">:" `elem` splitOn " " (text decodedMsg)) || ("<@" ++ x ++">" `elem` splitOn " " (text decodedMsg)))<$> uidList
        replies <- liftM (fmap (buildReply (channel decodedMsg) (user decodedMsg))) mentionedUsers
        void $ L8.putStrLn msg -- mostly for debug
        sendMessages connection replies
  --      if (channel decodedMsg == "C0460DZEC" )
  --        then replies >>= sequence . (sendMessages connection )
  --        else return [()]

    let loop = do
            line <- getLine
            unless (null line) $ do
                sendTextData connection (pack line)
                loop
    loop

    sendClose connection (pack "Bye!")

sendMessages :: Foldable t => WS.Connection -> t L.ByteString -> IO ()
sendMessages connection = traverse_ (sendTextData connection)

data AwayField = AwayField
                 { uid :: String
                 , reason ::String
                 } deriving (Show)

instance FromRow AwayField where
  fromRow = AwayField <$> field <*> field

instance ToRow AwayField where
  toRow (AwayField uid_ reason_) = toRow (uid_, reason_)

allAways :: IO [AwayField]
allAways = do
  conn <- open "/tmp/brbot.sqlite3"
  r <- query_ conn "SELECT * FROM away" :: IO [AwayField]
  close conn
  return r
