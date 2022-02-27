{-# LANGUAGE DeriveGeneric #-}
module Data.Schedule where

import           Network.Mirai                  ( sendMessage, Connection )
import           Codec.Serialise                ( Serialise, deserialiseOrFail, serialise )
import           GHC.Generics                   ( Generic )
import           Control.Exception              ( try, SomeException )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever, void )
import           Data.Either                    ( fromRight )
import           Data.Text                      ( Text )
import           Data.Foldable                  ( traverse_ )
import           Data.IORef                     ( readIORef, IORef, newIORef, writeIORef )
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString         as BS
import qualified Data.Text               as T
import qualified Data.Bifunctor
import           Data.Bifunctor                 ( second )
import           Data.Time                      ( getZonedTime, ZonedTime (zonedTimeToLocalTime), LocalTime (localTimeOfDay), TimeOfDay (todHour) )
import qualified Type.Mirai.Common       as CT  ( ChatType(Group, Friend), ChainMessage(ChainMessage))
import           Type.Mirai.Common              ( ChainMessage(ChainMessage))
import           Type.Mirai.Request
import           Module.Holiday                 ( getHolidayText_out)
import           Module.IllustrationFetch       ( fetchYandeRe24h_out )
import           Util.Log                       ( logErr )

type FuncName = String

type Schedule = IORef ScheduleTable

newtype ScheduleTable = ScheduleTable [(FuncName, [Target])]
  deriving (Generic, Show)
instance Serialise ScheduleTable

type Microsecond = Int
oneMin :: Microsecond
oneMin = 60000000


data Task = Task {
    procName :: String
  , runHour :: Int
  , runWeekDay :: [Int]
  , intervalDay :: Int
}


sendTarget :: Connection -> [ChainMessage] -> Target -> IO ()
sendTarget conn cm (User uid)  = sendMessage CT.Friend conn (Just . RSendMsg $ defSendMsg { sm_messageChain = cm, sm_qq = Just uid })
sendTarget conn cm (Group gid) = sendMessage CT.Group  conn (Just . RSendMsg $ defSendMsg { sm_messageChain = cm, sm_group = Just gid })

runSchedule :: Schedule -> Connection -> IO a
runSchedule scheRef conn = forever . void $ ((try $ do
          let sleep' = do
                currHour <- todHour . localTimeOfDay . zonedTimeToLocalTime <$> getZonedTime
                if currHour /= 21
                   then threadDelay (oneMin * 60) >> sleep'
                   else pure ()
          sleep'
          let singleRun (funcName, targets) = do
                rst <- getFunc funcName
                sequence $ (sendTarget conn) <$> rst <*> targets
          (ScheduleTable sche) <- readIORef scheRef
          traverse_ singleRun sche
          threadDelay (oneMin * 60 * 24)
       ) :: IO (Either SomeException ()))

getFunc :: String -> IO [[ChainMessage]]
getFunc funcName = case lookup funcName scheduleFuncMap of
  Just f -> f
  _ -> pure []

rmSchedule :: Target -> String -> Schedule -> IO (Either String ())
rmSchedule t funcName scheRef = do
  case lookup funcName scheduleFuncMap of
    Nothing -> pure (Left "not found")
    Just x -> do
      (ScheduleTable st) <- readIORef scheRef
      case removeIt (False, st) of
        (False, rst) -> pure $ Left "NOT yet in schedule"
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
    Nothing -> pure (Left "not found")
    Just _ -> do
      (ScheduleTable st) <- readIORef scheRef
      case replaceIt st of
        Nothing -> pure $ Left "ALREADY in schedule"
        Just x -> Right <$> writeIORef scheRef (ScheduleTable x)

  where
    replaceIt ::  [(FuncName, [Target])] -> Maybe [(FuncName, [Target])]
    replaceIt [] = Just [(funcName, [t])]
    replaceIt (x@(fn, targets):xs) = if fn == funcName
                                    then if t `elem` targets then Nothing else Just ((fn, t:targets):xs)
                                    else (x :) <$> replaceIt xs




-- scheduleFuncMap :: [(String, IO (Maybe Text))]
-- scheduleFuncMap = [("getHolidayText", getHolidayText)]

scheduleFuncMap :: [(String, IO [[ChainMessage]])]
scheduleFuncMap = [ ("getHolidayText",  getHolidayText_out)
                  , ("fetchYandeRe24h", fetchYandeRe24h_out) ]

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
