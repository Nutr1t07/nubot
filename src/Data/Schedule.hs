{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}
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
import qualified Data.List               as L
import qualified Data.Text.Read          as T
import qualified Data.Bifunctor
import           Data.Bifunctor                 ( second )
import           Data.Time
import           Data.Time.Calendar.WeekDate
import qualified Type.Mirai.Common       as CT  ( ChatType(Group, Friend), ChainMessage(ChainMessage))
import           Type.Mirai.Common              ( ChainMessage(ChainMessage))
import           Type.Mirai.Request
import           Module.Holiday                 ( getHolidayText_out )
import           Module.IllustrationFetch       ( fetchYandeRe24h_out, fetchYandeReWeek_out )
import           Module.Weather                 ( getRainyDay_out )
import           Util.Log                       ( logErr )
import           Util.Misc                      ( showT )

type TaskListRef = IORef TaskList


-- in microsecond
oneMin :: Int
oneMin = 60000000


getFunc :: String -> IO [[ChainMessage]]
getFunc funcName = case lookup funcName funcMap of
  Just f -> f
  _ -> pure []

funcMap :: [(String, IO [[ChainMessage]])]
funcMap = [ ("getHolidayText",  getHolidayText_out)
          , ("getRainyDay", getRainyDay_out)
          , ("fetchYandeRe24h", fetchYandeRe24h_out)
          , ("fetchYandeReWeek", fetchYandeReWeek_out)
          ]

---------------------------------------------------------------------------------------------------
-- Task

newtype TaskList = TaskList [Task]
  deriving (Generic, Show)
instance Serialise TaskList

data Task = Task {
    procName   :: String
  , runTime    :: TimeInfo
  , target     :: Target
} deriving (Generic, Show)
instance Serialise Task

data Target = User Integer | Group Integer
  deriving (Generic, Eq, Show)
instance Serialise Target

runScheduledTask :: TaskListRef -> Connection -> IO a
runScheduledTask taskRef conn = forever . void $ ((try $ do
          let singleRun (Task funcName timeInfo target') = do
                runFlag <- checkTimeSatisfied timeInfo
                if not runFlag
                  then pure []
                  else do rst <- getFunc funcName
                          sequence $ (sendTarget conn) <$> rst <*> pure target'
          (TaskList tasks) <- readIORef taskRef
          traverse_ singleRun tasks
          threadDelay oneMin
       ) :: IO (Either SomeException ()))
  where
    sendTarget conn cm (User uid)  = sendMessage CT.Friend conn (Just . RSendMsg $ defSendMsg { sm_messageChain = cm, sm_qq = Just uid })
    sendTarget conn cm (Group gid) = sendMessage CT.Group  conn (Just . RSendMsg $ defSendMsg { sm_messageChain = cm, sm_group = Just gid })

rmScheduledTask :: String -> Target -> TaskListRef -> IO (Either String ())
rmScheduledTask funcName t taskListRef = do
  case lookup funcName funcMap of
    Nothing -> pure (Left "not found")
    Just x -> do
      (TaskList taskList) <- readIORef taskListRef
      Right <$> writeIORef taskListRef (TaskList $ removeIt taskList)

  where
    removeIt [] = []
    removeIt (x:xs) = if target x == t && procName x == funcName
                         then xs
                         else x:(removeIt xs)

addScheduledTask :: TimeInfo -> String -> Target -> TaskListRef -> IO (Either String ())
addScheduledTask timeInfo funcName t taskListRef = do
  case lookup funcName funcMap of
    Nothing -> pure (Left "not found")
    Just _ -> do
      (TaskList taskList) <- readIORef taskListRef
      Right <$> writeIORef taskListRef (TaskList $ addIt taskList)
  where
    addIt [] = [Task funcName timeInfo t]
    addIt xss@(x:xs) = if target x /= t || procName x /= funcName
                          then x : addIt xs
                          else xss


readTaskList :: IO (Maybe (TaskListRef))
readTaskList = do
  raw <- fromRight BS.empty <$> (try (BS.readFile taskListPath) :: IO (Either SomeException BS.ByteString))
  case deserialiseOrFail (BL.fromStrict raw) of
    Right x  -> Just <$> newIORef x
    Left err -> logErr "reading scheduled tasks from local file" (show err) >> pure Nothing

emptyTaskList :: IO TaskListRef
emptyTaskList = newIORef $ TaskList []

saveTaskList :: TaskListRef -> IO ()
saveTaskList taskListRef = do
  schTable <- readIORef taskListRef
  BL.writeFile taskListPath $ serialise schTable

taskListPath :: [Char]
taskListPath = "taskList.dat"


---------------------------------------------------------------------------------------------------
-- TimeInfo

data TimeInfo = TimeInfo {
    tMin       :: TimeField
  , tHour      :: TimeField
  , tDayOfMo   :: TimeField
  , tMonth     :: TimeField
  , tDayOfWeek :: TimeField
} deriving (Generic)
instance Serialise TimeInfo
instance Show TimeInfo where
  show TimeInfo{..} = mconcat $ L.intersperse " " $ show <$> [tMin, tHour, tDayOfMo, tMonth, tDayOfWeek]

data TimeField = MatchAll | MatchSome [BaseField]
  deriving (Generic)
instance Serialise TimeField
instance Show TimeField where
  show MatchAll = "*"
  show (MatchSome xs) = mconcat $ L.intersperse "," $ show <$> xs

data BaseField = SingleField Int | RangeField Int Int
  deriving (Generic)
instance Serialise BaseField
instance Show BaseField where
  show (SingleField x) = show x
  show (RangeField x y) = show x <> "-" <> show y

checkTimeSatisfied :: TimeInfo -> IO Bool
checkTimeSatisfied TimeInfo{..} = do
    currTime <- zonedTimeToLocalTime <$> getZonedTime
    let [cMin, cHour] = [todMin, todHour] <*> (pure $ localTimeOfDay currTime)
        (_, cMonth, cDayOfMo) = toGregorian $ localDay currTime
        (_, _, cDayOfWeek) = toWeekDate $ localDay currTime
    currHour <- todHour . localTimeOfDay . zonedTimeToLocalTime <$> getZonedTime

    pure $ all id [ checkTimeField cMin tMin
         , checkTimeField cHour tHour
         , checkTimeField cDayOfMo tDayOfMo
         , checkTimeField cMonth tMonth
         , checkTimeField cDayOfWeek tDayOfWeek]

  where
    checkTimeField _ MatchAll = True
    checkTimeField curr (MatchSome xs)  = any (checkBaseField curr) xs

    checkBaseField curr (SingleField x)  = x == curr
    checkBaseField curr (RangeField min max) = curr >= min && curr <= max

parseTimeInfo :: Text -> Either String TimeInfo
parseTimeInfo str =
  let raw = split' ' ' str in
  if length raw /= 5
    then Left "argument length exceeded, expected 5"
    else
      fmap promote $ sequence =<< fmap (zipWith (\f x -> f x)
              [guardTF 0 59, guardTF 0 23, guardTF 1 31, guardTF 1 12, guardTF 1 7])
              (traverse parseSingle raw)

  where
    split' x = filter (/="") . T.split (==x)
    elem' x xs = T.any (==x) xs

    promote [a,b,c,d,e] = TimeInfo a b c d e

    parseSingle :: Text -> Either String TimeField
    parseSingle xs =
      if xs == "*"
        then Right MatchAll
        else let items = split' ',' xs in
             fmap MatchSome $ traverse (\x -> if '-' `elem'` x
                       then (case split' '-' x of
                              (min : max : _) ->
                                   let min' = fst <$> T.decimal min
                                       max' = fst <$> T.decimal max in
                                   RangeField <$> min' <*> max'
                              _ -> Left "spliting range field failed")
                       else SingleField <$> fst <$> T.decimal x) items

    guardTF min max xss@(MatchSome xs) = MatchSome <$> traverse (guardBF min max) xs
    guardTF _ _ MatchAll = Right MatchAll

    guardBF min max field@(SingleField x) = if min <= x && max >= x then Right field else Left $ show field <> " out of range [" <> show min <> ", " <> show max <> "]"
    guardBF min max field@(RangeField l r)
      | l == r    = Left "RangeField duplicate"
      | l > r     = guardBF min max (RangeField r l)
      | otherwise = if min <= l && max >= r then Right field else Left $ show field <> " out of range [" <> show min <> ", " <> show max <> "]"
---------------------------------------------------------------------------------------------------


