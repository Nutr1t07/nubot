{-# LANGUAGE DeriveGeneric #-}
module Data.Schedule where

import qualified Data.ByteString.Lazy as BL
import Codec.Serialise ( Serialise, deserialiseOrFail, serialise )
import GHC.Generics ( Generic )
import qualified Data.ByteString as BS
import Control.Exception ( try, SomeException )
import Util.Log ( logErr )
import Data.Either ( fromRight )
import Module.Holiday (getHolidayText)
import Data.Text (Text)
import Control.Concurrent (threadDelay)
import Control.Monad ( forever, void )
import Data.Mirai (mkSendMsgT)
import Network.Mirai ( sendMessage, Connection )
import qualified Type.Mirai.Common as CT ( ChatType(Group, Friend) )
import Data.Foldable (traverse_)
import Data.IORef (readIORef, IORef, newIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Bifunctor
import Data.Bifunctor (second)
import Data.Time (getZonedTime, ZonedTime (zonedTimeToLocalTime), LocalTime (localTimeOfDay), TimeOfDay (todHour))

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
runSchedule scheRef conn = forever . void $ ((try $ do
          currHour <- todHour .localTimeOfDay . zonedTimeToLocalTime <$> getZonedTime
          let sleepTo10 = if currHour /= 11 then threadDelay (oneMin * 60) >> sleepTo10 else pure ()
          sleepTo10
          let singleRun (funcName, targets) = do
                rst <- getFunc funcName
                case rst of
                  Just txt -> sendTarget conn txt `traverse_` (targets:: [Target])
                  _ -> pure ()
          (ScheduleTable sche) <- readIORef scheRef
          traverse_ singleRun sche
          threadDelay (oneMin * 60 * 24)
       ) :: IO (Either SomeException ()))

getFunc :: String -> IO (Maybe Text)
getFunc funcName = case lookup funcName scheduleFuncMap of
  Just f -> f
  _ -> pure Nothing

rmSchedule :: Target -> String -> Schedule -> IO (Either String ())
rmSchedule t funcName scheRef = do
  case lookup funcName scheduleFuncMap of
    Nothing -> pure (Left "not found.")
    Just x -> do
      (ScheduleTable st) <- readIORef scheRef
      case removeIt (False, st) of
        (False, rst) -> pure $ Left "this target is NOT in schedule"
        (True, rst) -> Right <$> writeIORef scheRef (ScheduleTable rst)

  where
    removeIt (b,  []) = (b, [])
    removeIt (b, x@(fn, targets):xs) = if t `elem` targets
                                          then (True, (fn, filter (/= t) targets):xs)
                                          else
                                            case removeIt (b, xs) of
                                              (True, xxx) -> (True, x:xxx)
                                              (False, xxx) -> second (x :) $ removeIt (b, xxx)


addSchedule :: Target -> String -> Schedule -> IO (Either String ())
addSchedule t funcName scheRef = do
  case lookup funcName scheduleFuncMap of
    Nothing -> pure (Left "no corresponding schedule func found.")
    Just _ -> do
      (ScheduleTable st) <- readIORef scheRef
      case replaceIt st of
        Nothing -> pure $ Left "this target is ALREADY in schedule"
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
