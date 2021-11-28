module Main where

import Network.Mirai
import Util.Log
import Data.Aeson
import Data.ByteString.Lazy
import Data.Text
import Control.Exception
import Prelude hiding (readFile)
import Util.Config
import Data.Maybe
import Control.Monad
import Type.Mirai.Update
import AutoReply.Handler
import Data.TaskQueue (emptyTaskQueue)
import Data.User
import Data.Mirai (initMsgLogDB)
import GHC.IO.Encoding

main :: IO ()
main = do
  setLocaleEncoding utf8
  initMsgLogDB
  cfg <- readConfig
  when (isJust cfg) $ do
    userGroup <- getUserGroup
    taskQueue <- emptyTaskQueue (saveUserGroup userGroup)
    repliedTable <- emptyRepliedTable
    runConn (fromJust cfg)
      (mainHandler (mirai_qq_id $ fromJust cfg) taskQueue userGroup repliedTable)

getUserGroup :: IO UserGroup
getUserGroup = do
  grp <- readUserGroup  -- read from local file
  maybe emptyUserGroup pure grp

readConfig :: IO (Maybe Config)
readConfig = let tag = "parsing config" in do
  rawCfg <- try $ readFile "config.json" :: IO (Either SomeException ByteString)
  case rawCfg of
    Left err -> logErr tag (show err) >>= const (pure Nothing)
    Right cfgRaw' -> case eitherDecode cfgRaw' of
      Left err -> logErr tag (show err) >>= const (pure Nothing)
      Right cfg -> pure (Just cfg)