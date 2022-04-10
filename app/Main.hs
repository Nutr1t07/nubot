module Main where

import Control.Exception    ( try, SomeException (SomeException) )
import Control.Monad        ( Monad((>>=)), when )
import Data.Aeson           ( eitherDecode )
import Data.ByteString.Lazy ( readFile, ByteString )
import Data.Maybe
import GHC.IO.Encoding      ( utf8, setLocaleEncoding )

import AutoReply.Handler    ( mainHandler )
import Data.TaskQueue       ( emptyTaskQueue )
import Data.Either.Combinators
import Data.User
                            ( UserGroup,
                              emptyRepliedTable,
                              emptyUserGroup,
                              getUserGroupFromLocal,
                              saveUserGroup )
import Data.Mirai           ( initMsgLogDB )
import Data.Schedule        ( TaskListRef, emptyTaskList, getTaskListFromLocal, runScheduledTask )
import Network.Mirai        ( runConn )
import Util.Config          ( Config(mirai_qq_id) )
import Util.Log             ( logErr )

import Prelude hiding (readFile)

main :: IO ()
main = do
  setLocaleEncoding utf8
  initMsgLogDB
  cfg <- readConfig
  case cfg of
    Nothing -> logErr "reading config" "empty"
    Just cfg' -> do
      userGroup     <- maybe emptyUserGroup pure =<< getUserGroupFromLocal
      scheduledTask <- maybe emptyTaskList pure =<< getTaskListFromLocal
      taskQueue     <- emptyTaskQueue (saveUserGroup userGroup)
      repliedTable  <- emptyRepliedTable
      runConn cfg'
        (mainHandler (mirai_qq_id cfg') taskQueue userGroup scheduledTask repliedTable)
        (runScheduledTask scheduledTask)

readConfig :: IO (Maybe Config)
readConfig = do
  rawCfg <- mapLeft show <$>
              (try (readFile "config.json") :: IO (Either SomeException ByteString))
  case rawCfg >>= eitherDecode of
    Left err -> logErr "parsing config" (show err) >> pure Nothing
    Right cfg -> pure (Just cfg)
