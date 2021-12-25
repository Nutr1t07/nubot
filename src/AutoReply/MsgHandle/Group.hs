{-# LANGUAGE OverloadedStrings #-}
module AutoReply.MsgHandle.Group where

import Network.Mirai ( sendMessage )
import Data.Mirai
    ( getPlainText,
      getImgUrls',
      transUpd2SendMsgTQ,
      getChatType,
      transUpd2SendMsgT,
      transUpd2SendMsgPQ, getMessageTime, getGroupId )
import Module.ImageSearch ( runSauceNAOSearch )
import AutoReply.HandleEnv ( HandleEnv(update, connection, schedule) )
import Data.Monads ( ReaderT, MonadTrans(lift), asks )
import Util.Log ( logWT, LogTag(Info) )
import Data.Maybe ( fromMaybe, fromJust )
import qualified Data.Text as T
import AutoReply.Misc (trimT, trimT')
import Module.WebSearch (runBaiduSearch)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Util.Misc (showT)
import Data.Schedule (scheduleFuncMap, Target (Group), addSchedule, saveSchedule)
import Data.IORef (readIORef)

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
                       Just x -> showT (ct - x)
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

addScheduleHdl :: ReaderT HandleEnv IO ()
addScheduleHdl = do
  upd <- asks update
  let msgTxt = fromMaybe "" (getPlainText upd)
      funcName = getText msgTxt
  case getGroupId upd of
    Nothing -> replyQ "unable to fetch group id"
    Just gid -> case lookup (T.unpack funcName) scheduleFuncMap of
          Nothing -> replyQ "no corresponding schedule plan found."
          Just x -> do
            schRef <- asks schedule
            rst <- lift $ addSchedule (Group gid) (T.unpack funcName) schRef
            case rst of
              Right () -> replyQ "success" >> lift (saveSchedule schRef)
              Left err -> replyQ $ T.pack err
  pure ()
  where
    getText txt = trimT' " \n" $ T.dropWhile (== ' ') $ T.dropWhile (/= ' ') txt

getScheduleHdl :: ReaderT HandleEnv IO ()
getScheduleHdl = do
  schRef <- asks schedule
  sche <- lift $ readIORef schRef
  reply (showT sche)

reply' f = do
  upd <- asks update
  conn <- asks connection
  lift $ sendMessage (fromJust $ getChatType upd) conn (f upd)
reply text = reply' (`transUpd2SendMsgT` text)
replyQ text = reply' (`transUpd2SendMsgTQ` text)
replyPicQ text url = reply' (\upd -> transUpd2SendMsgPQ upd text url)