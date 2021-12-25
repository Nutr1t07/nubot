{-# LANGUAGE DeriveGeneric #-}
module Data.Schedule where

import qualified Data.ByteString.Lazy as BL
import Codec.Serialise
import GHC.Generics
import qualified Data.ByteString as BS
import Control.Exception
import Util.Log
import Data.Either
import Module.Holiday (getHolidayText)
import Data.Text (Text)
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Mirai (mkSendMsgT)
import Network.Mirai
import qualified Type.Mirai.Common as CT ( ChatType(Group, Friend) )
import Data.Foldable (traverse_)
import Data.IORef (readIORef, IORef, newIORef, writeIORef)
import qualified Data.Text as T

-- should be functions that has Maybe Int -> Maybe Int -> IO () as type
type FuncName = String

type Schedule = IORef ScheduleTable

newtype ScheduleTable = ScheduleTable [(FuncName, [Target])]
  deriving (Generic, Show)
instance Serialise ScheduleTable

type Microsecond = Int
oneMin :: Microsecond
oneMin = 60000000

sendTarget :: Connection -> Text -> Target -> IO ()
sendTarget conn txt (User uid)  = sendMessage CT.Friend conn (Just (mkSendMsgT (Just uid) Nothing txt))
sendTarget conn txt (Group gid) = sendMessage CT.Group conn (Just (mkSendMsgT Nothing (Just gid) txt))

runSchedule :: Schedule -> Connection -> IO a
runSchedule scheRef conn = do
  (ScheduleTable sche) <- readIORef scheRef
  forever . void
    $ ((try $ do
          let singleRun (funcName, targets) = do
                rst <- getFunc funcName
                case rst of
                  Just txt -> sendTarget conn txt `traverse_` (targets:: [Target])
                  _ -> pure ()
          traverse_ singleRun sche
          threadDelay (oneMin * 60 * 24)
       ) :: IO (Either SomeException ())
      )

getFunc :: String -> IO (Maybe Text)
getFunc funcName = case lookup funcName scheduleFuncMap of
  Just f -> f
  _ -> pure Nothing

addSchedule :: Target -> String -> Schedule -> IO (Either String ())
addSchedule t funcName scheRef = do
  case lookup funcName scheduleFuncMap of
    Nothing -> pure (Left "no corresponding schedule func found.")
    Just x -> do
      (ScheduleTable st) <- readIORef scheRef
      case replaceIt st of
        Nothing -> pure $ Left "this target is already in schedule"
        Just x -> Right <$> writeIORef scheRef (ScheduleTable x)

  where
    replaceIt ::  [(FuncName, [Target])] -> Maybe [(FuncName, [Target])]
    replaceIt [] = Just [(funcName, [t])]
    replaceIt (x@(fn, targets):xs) = if fn == funcName
                                    then if t `elem` targets then Nothing else Just ((fn, t:targets):xs)
                                    else (x :) <$> replaceIt xs




scheduleFuncMap :: [(String, IO (Maybe Text))]
scheduleFuncMap = [("getHolidayText", getHolidayText)]

schedulePath :: [Char]
schedulePath = "schedule.dat"

emptySchedule :: IO Schedule
emptySchedule = newIORef $ ScheduleTable []

saveSchedule :: Schedule -> IO ()
saveSchedule sch = do
  schTable <- readIORef sch
  BL.writeFile schedulePath $ serialise schTable

readSchedule :: IO (Maybe Schedule)
readSchedule = do
  raw <- fromRight BS.empty <$> (try (BS.readFile schedulePath) :: IO (Either SomeException BS.ByteString))
  case deserialiseOrFail (BL.fromStrict raw) of
    Right x  -> Just <$> newIORef x
    Left err -> logErr "reading schedule from local file" (show err) >> pure Nothing

data Target = User Integer | Group Integer
  deriving (Generic, Eq, Show)
instance Serialise Target
