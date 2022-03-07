{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Mirai where

import Control.Monad (forever, void, replicateM_)
import Data.Aeson ( encode, decode, Value, ToJSON (toJSON), eitherDecode )
import qualified Network.WebSockets as WS

import Type.Mirai.Update ( Update, WebsocketWrapper (WebsocketWrapper, wrap_data) )
import Util.Config ( Config(..) )
import Type.Mirai.Request (RequestContent)
import GHC.Generics (Generic)
import Data.ByteString.Lazy ( ByteString, toStrict )
import qualified Data.ByteString.Char8 as Char8
import Type.Mirai.Common (ChatType)
import Util.Json ( dropToJSON )
import Util.Log ( logWT, LogTag(..), logWT'C8, logErr )
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (try, SomeException (SomeException))

type Connection = WS.Connection

runConn :: Config -> (Update -> Connection -> IO ()) -> (Connection -> IO ()) -> IO ()
runConn Config{..} f prepareAct = WS.runClient ws_addr ws_port path app
  where
      path = "/all?verifyKey=" <> mirai_verify_key
                  <> "&qq="    <> show mirai_qq_id

      app conn = do
          logWT Info "websocket connected"
          forkIO $ prepareAct conn

          forever $ do
            raw <- WS.receiveData conn :: IO ByteString
            logWT'C8 Debug ("received message: " <> toStrict raw)
            let msg = eitherDecode raw :: (Either String WebsocketWrapper)

            -- msg <- decode <$> WS.receiveData conn :: IO (Maybe Update)

            case msg of
              Right wsdata -> do
                exWrap $ f (wrap_data wsdata) conn
              Left err -> logErr "parsing data from websocket" err >> pure ()

      exWrap f = do
                  a <- try f :: IO (Either SomeException ())
                  case a of
                    Left err -> logErr "critical error" (show err)
                    _ -> pure ()

sendMessage :: ChatType -> Connection -> Maybe RequestContent -> IO ()
sendMessage = (((threadDelay 2000000 >>) .) .) . sendMessage'  -- send with a delay of 3 sec by default

sendMessage' :: ChatType -> Connection -> Maybe RequestContent -> IO ()
sendMessage' chatType = sendCommand  ("send" <> show chatType <> "Message")

sendCommand :: String -> Connection -> Maybe RequestContent -> IO ()
sendCommand cmd conn val = case val of
  Just val' -> do
    let wrapped = encode $ WSWrap 0 cmd Nothing val'
    logWT'C8 Debug ("sending message: " <> toStrict wrapped)
    WS.sendTextData conn wrapped
  Nothing -> logWT'C8 Debug "message didn't send: Nothing"


data WSWrap = WSWrap {
    wrap_syncId :: Int
  , wrap_command :: String
  , wrap_subCommand :: Maybe String
  , wrap_content :: RequestContent
} deriving (Generic, Show)
instance ToJSON WSWrap where
  toJSON = dropToJSON 5