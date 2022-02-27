{-# LANGUAGE OverloadedStrings #-}
module AutoReply.MsgHandle.Common where

import           Network.Mirai                    ( sendMessage )
import           Data.Mirai                       ( getPlainText,
                                                    getImgUrls',
                                                    transUpd2SendMsgTQ,
                                                    transUpd2SendMsgPBase64,
                                                    getChatType,
                                                    transUpd2SendMsgT,
                                                    transUpd2SendMsgPQ, getMessageTime, getGroupId, getUserId )
import           AutoReply.HandleEnv              ( HandleEnv(update, connection, schedule) )
import           AutoReply.Misc                   ( trimT, trimT')
import           Data.Monads                      ( ReaderT, MonadTrans(lift), asks )
import           Data.Maybe                       ( fromMaybe, fromJust )
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Text.Encoding.Base64  as T  ( encodeBase64 )
import           Data.Time.Clock.POSIX            ( getPOSIXTime)
import           Data.Schedule                    ( scheduleFuncMap, Target (Group, User), addSchedule, saveSchedule, rmSchedule, Schedule)
import           Data.IORef                       ( readIORef)
import           Util.Log                         ( logWT, LogTag(Info) )
import           Util.Misc                        ( showT )
import           Control.Exception
import           Module.ImageSearch               ( runSauceNAOSearch )
import           Module.WebSearch                 ( runBaiduSearch, runGoogleSearch )
import           Module.Weather                   ( write7DayScreenshot, getNextRainyDay )
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
    Right (url, txt, Just more) -> replyPicQ txt url >> replyQ more
    Left err -> replyQ err

pingHdl :: ReaderT HandleEnv IO ()
pingHdl = do
  ct <- lift $ round . (* 1000) <$> getPOSIXTime
  upd <- asks update
  let msgTime = (* 1000) <$> getMessageTime upd
  let timeInterval = case msgTime of
                       Just x -> let x' = ct - x' in showT $ if x' < 0 then 0 - x' else x'
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
      reply $ title <> "\n\n" <> abstract <> "\n\n" <> link <> "\n\n" <> searchLink
    Nothing -> pure ()
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
      reply $ title <> "\n\n" <> abstract <> "\n\n" <> link
    Nothing -> pure ()
  where
    getQueryText txt = trimT' " \n" $ T.dropWhile (/= ' ') txt

_addOrRemoveSchedule :: (Target -> String -> Schedule -> IO (Either String ()))
                            -> ReaderT HandleEnv IO ()
_addOrRemoveSchedule f = do
  upd <- asks update
  let msgTxt = fromMaybe "" (getPlainText upd)
      funcName = getText msgTxt
  case (getGroupId upd, getUserId upd) of
    (Nothing, Nothing)  -> replyQ "unable to fetch group id or user id"
    (Nothing, Just uid) -> do
            schRef <- asks schedule
            rst <- lift $ f (User uid) (T.unpack funcName) schRef
            case rst of
              Right () -> replyQ "success" >> lift (saveSchedule schRef)
              Left err -> replyQ $ T.pack err
    (Just gid, _) -> do
            schRef <- asks schedule
            rst <- lift $ f (Group gid) (T.unpack funcName) schRef
            case rst of
              Right () -> replyQ "success" >> lift (saveSchedule schRef)
              Left err -> replyQ $ T.pack err
  pure ()
  where
    getText txt = trimT' " \n" $ T.dropWhile (== ' ') $ T.dropWhile (/= ' ') txt

addScheduleHdl :: ReaderT HandleEnv IO ()
addScheduleHdl = _addOrRemoveSchedule addSchedule

rmScheduleHdl :: ReaderT HandleEnv IO ()
rmScheduleHdl = _addOrRemoveSchedule rmSchedule

getScheduleHdl :: ReaderT HandleEnv IO ()
getScheduleHdl = do
  schRef <- asks schedule
  sche <- lift $ readIORef schRef
  reply (showT sche)

getWeatherHdl :: ReaderT HandleEnv IO ()
getWeatherHdl = do
  txt <- lift getNextRainyDay
  case txt of
    Just x -> reply x
    _ -> pure ()
  ret <- lift $ write7DayScreenshot
  case ret of
    False -> replyQ "从网络获取天气图像失败。"
    True -> do
      picContent <- lift $ catch (encodeBase64 <$> T.readFile "screenshot.png") (\x -> const (pure "") (x::SomeException))
      case picContent of
        "" -> replyQ "读取天气图像文件失败。"
      replyPicBase64 "" picContent


reply' f = do
  upd <- asks update
  conn <- asks connection
  lift $ sendMessage (fromJust $ getChatType upd) conn (f upd)
reply text = reply' (`transUpd2SendMsgT` text)
replyQ text = reply' (`transUpd2SendMsgTQ` text)
replyPicQ text url = reply' (\upd -> transUpd2SendMsgPQ upd text url)
replyPicBase64 text picBase64 = reply' (\upd -> transUpd2SendMsgPBase64 upd text picBase64)