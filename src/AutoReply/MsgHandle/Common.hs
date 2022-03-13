{-# LANGUAGE OverloadedStrings #-}
module AutoReply.MsgHandle.Common where

import           Network.Mirai                    ( sendMessage )
import           Data.Mirai
import           AutoReply.HandleEnv
import           AutoReply.Misc                   ( trimT, trimT')
import           Data.Monads                      ( ReaderT, MonadTrans(lift), asks )
import           Data.Maybe                       ( fromMaybe, fromJust )
import           Data.Foldable                    ( traverse_ )
import qualified Data.Text                  as T
import           Data.Text.Encoding         as T  ( decodeUtf8 )
import qualified Data.ByteString            as BS
import qualified Data.Text.IO               as T
import           Data.ByteString.Base64           ( encodeBase64 )
import           Data.Time.Clock.POSIX            ( getPOSIXTime)
import           Data.Schedule
import           Data.IORef
import           Util.Log                         ( logWT, LogTag(Info, Debug, Error) )
import           Util.Misc                        ( showT )
import           Control.Exception
import           Module.ImageSearch               ( runSauceNAOSearch, getYandexScreenshot )
import           Module.WebSearch                 ( runBaiduSearch, runBaikeSearch, runGoogleSearch )
import           Module.Weather                   ( get7DayScreenshot, getNextRainyDay )
import           Module.IllustrationFetch         ( fetchYandeRe24h )

searchImageHdl :: ReaderT HandleEnv IO ()
searchImageHdl = do
  upd <- asks update
  urls <- lift $ getImgUrls' upd
  let imgUrl = case urls of
        [] -> T.empty
        (x:_)  -> x

  lift $ logWT Info $ "[searchImageHdl] searching image: " <> show imgUrl
  rst <- lift $ runSauceNAOSearch imgUrl
  case rst of
    Right (url, txt, Nothing) -> replyPicQ txt url
    Right (url, txt, Just more) -> do
      replyPicQ txt url
      replyQ more
      imgBase64' <- lift $ getYandexScreenshot imgUrl
      case imgBase64' of
        Just x -> replyPicBase64 "# Yandex 网页搜索" x
        _ -> pure ()
    Left err -> replyQ err

pingHdl :: ReaderT HandleEnv IO ()
pingHdl = do
  ct <- lift $ round . (* 1000) <$> getPOSIXTime
  upd <- asks update
  let msgTime = (* 1000) <$> getMessageTime upd
  let timeInterval = case msgTime of
                       Just x -> let x' = ct - x in showT $ if x' < 0 then 0 - x' else x'
                       Nothing -> "UNKNOWN"
  replyQ $ "1 packet transmitted, 1 received, 0% packet loss, time " <> timeInterval <> "ms"

searchBaiduHdl :: ReaderT HandleEnv IO ()
searchBaiduHdl = do
  upd <- asks update
  let msgTxt = fromMaybe "" (getPlainText upd)
  let query = getQueryText msgTxt
  lift $ logWT Info $ "[searchBaiduHdl] searching text: " <> show query
  baiduRst <- lift $ runBaiduSearch query
  case baiduRst of
    Just (link, title, abstract, searchLink) ->
      replyQ $ title <> "\n\n" <> abstract <> "\n\n" <> link <> "\n\n" <> searchLink
    Nothing -> pure ()
  where
    getQueryText txt = trimT' " \n" $ T.dropWhile (/= ' ') txt

searchBaikeHdl :: ReaderT HandleEnv IO ()
searchBaikeHdl = do
  upd <- asks update
  let msgTxt = fromMaybe "" (getPlainText upd)
  let query = getQueryText msgTxt
  baikeRst <- lift $ runBaikeSearch query
  case baikeRst of
    (_       , Nothing)  -> pure ()
    (Just txt, Just url) -> replyQ $ txt <> "\n\n" <> url
    (Nothing , Just url) -> reply $ "查询到结果，但没有摘要，请访问该页面以查看结果\n\n" <> url
  where
    getQueryText txt = trimT' " \n" $ T.dropWhile (/= ' ') txt

searchGoogleHdl :: ReaderT HandleEnv IO ()
searchGoogleHdl = do
  upd <- asks update
  let msgTxt = fromMaybe "" (getPlainText upd)
  let query = getQueryText msgTxt
  lift $ logWT Info $ "[searchGoogleHdl] searching text: " <> show query
  baiduRst <- lift $ runGoogleSearch query
  case baiduRst of
    Just (link, title, abstract) ->
      replyQ $ title <> "\n\n" <> abstract <> "\n\n" <> link
    Nothing -> pure ()
  where
    getQueryText txt = trimT' " \n" $ T.dropWhile (/= ' ') txt

_addOrRemoveSchedule :: (Target -> TaskListRef -> IO (Either String ()))
                            -> ReaderT HandleEnv IO ()
_addOrRemoveSchedule f = do
  upd <- asks update
  let msgTxt = fromMaybe "" (getPlainText upd)
  case (getGroupId upd, getUserId upd) of
    (Nothing, Nothing)  -> replyQ "unable to fetch group id or user id"
    (Nothing, Just uid) -> do
            taskListRef <- asks taskList
            rst <- lift $ f (User uid) taskListRef
            case rst of
              Right () -> replyQ "success" >> lift (saveTaskList taskListRef)
              Left err -> replyQ $ T.pack err
    (Just gid, _) -> do
            taskListRef <- asks taskList
            rst <- lift $ f (Group gid) taskListRef
            case rst of
              Right () -> replyQ "success" >> lift (saveTaskList taskListRef)
              Left err -> replyQ $ T.pack err
  pure ()
  where
    getText txt = trimT' " \n" $ T.dropWhile (== ' ') $ T.dropWhile (/= ' ') txt

addScheduleHdl :: ReaderT HandleEnv IO ()
addScheduleHdl = do
  upd <- asks update
  let words' = Prelude.filter (/="") $ T.split (==' ') $ fromMaybe "" (getPlainText upd)
  if length words' /= 7
      then replyQ "invalid argument number, expected 7"
      else do
        let funcName = words' !! 1
            timeInfo = parseTimeInfo $ T.unwords $ Prelude.drop 2 words'
        case timeInfo of
          Left err -> replyQ $ "error time pattern: " <> showT err
          Right timeInfo' -> _addOrRemoveSchedule (addScheduledTask timeInfo' (T.unpack funcName))
  where
    getText txt = trimT' " \n" $ T.dropWhile (== ' ') $ T.dropWhile (/= ' ') txt

rmScheduleHdl :: ReaderT HandleEnv IO ()
rmScheduleHdl = do
  upd <- asks update
  let funcName = getText $ fromMaybe "" (getPlainText upd)
  _addOrRemoveSchedule (rmScheduledTask (T.unpack funcName))
  where
    getText txt = trimT' " \n" $ T.dropWhile (== ' ') $ T.dropWhile (/= ' ') txt

getScheduleHdl :: ReaderT HandleEnv IO ()
getScheduleHdl = do
  taskListRef <- asks taskList
  taskList <- lift $ readIORef taskListRef
  reply (showT taskList)

getWeatherHdl :: ReaderT HandleEnv IO ()
getWeatherHdl = do
  txt <- lift getNextRainyDay
  case txt of
    Just x -> reply x
    _ -> reply "未来一周无雨。"
  pic <- lift $ get7DayScreenshot
  case pic of
    Nothing -> do
      lift $ logWT Error $ "[getWeatherHdl] getting screenshot failed"
      replyQ "获取天气图像失败。"
    Just x -> replyPicBase64 "" x

getYande24hHdl :: ReaderT HandleEnv IO ()
getYande24hHdl = do
  urls <- lift fetchYandeRe24h
  traverse_ (replyPic "") urls

reply' f = do
  upd <- asks update
  conn <- asks connection
  lift $ logWT Debug $ "sending message" <> show (f upd)
  lift $ sendMessage (fromJust $ getChatType upd) conn (f upd)
reply text = reply' (`transUpd2SendMsgT` text)
replyPic text url = reply' (\upd -> transUpd2SendMsgP upd text url)
replyQ text = reply' (`transUpd2SendMsgTQ` text)
replyPicQ text url = reply' (\upd -> transUpd2SendMsgPQ upd text url)
replyPicBase64 text picBase64 = reply' (\upd -> transUpd2SendMsgPBase64 upd text picBase64)