{-# LANGUAGE OverloadedStrings #-}
module AutoReply.MsgHandle.Group where

import Network.Mirai ( sendMessage )
import Data.Mirai
    ( getPlainText,
      getImgUrls',
      mkSendMsgTQ,
      getChatType,
      mkSendMsgT,
      mkSendMsgPQ )
import Module.ImageSearch ( runSauceNAOSearch )
import AutoReply.HandleEnv ( HandleEnv(update, connection) )
import Data.Monads ( ReaderT, MonadTrans(lift), asks )
import Util.Log ( logWT, LogTag(Info) )
import Data.Maybe ( fromMaybe, fromJust )
import qualified Data.Text as T
import AutoReply.Misc (trimT, trimT')
import Module.WebSearch (runBaiduSearch)

searchImageHdl :: ReaderT HandleEnv IO ()
searchImageHdl = do
  upd <- asks update
  urls <- lift $ getImgUrls' upd
  let imgUrl = case urls of
        [] -> T.empty
        (x:_)  -> x

  lift $ logWT Info $ "running SauceNAO search: " <> show imgUrl
  rst <- lift $ runSauceNAOSearch imgUrl
  case rst of
    Right (url, txt, Nothing) -> replyPicQ txt url
    Right (url, txt, Just more) -> replyPicQ txt url >> replyQ more
    Left err -> replyQ err

pingHdl :: ReaderT HandleEnv IO ()
pingHdl = replyQ "1 packets transmitted, 1 received, 0% packet loss"

searchBaiduHdl :: T.Text -> ReaderT HandleEnv IO ()
searchBaiduHdl cmd = do
  upd <- asks update
  let query = getQueryText $ fromMaybe "" (getPlainText upd)
  baiduRst <- lift $ runBaiduSearch query
  case baiduRst of
    Just (link, title, abstract, searchLink) ->
      reply $ title <> "\n\n" <> abstract <> "\n\n" <> link <> "\n\n" <> searchLink
    Nothing -> pure ()
  where
    getQueryText txt = trimT' " \n" $ T.drop (T.length cmd) txt

reply' f = do
  upd <- asks update
  conn <- asks connection
  lift $ sendMessage (fromJust $ getChatType upd) conn (f upd)
reply text = reply' (`mkSendMsgT` text)
replyQ text = reply' (`mkSendMsgTQ` text)
replyPicQ text url = reply' (\upd -> mkSendMsgPQ upd text url)