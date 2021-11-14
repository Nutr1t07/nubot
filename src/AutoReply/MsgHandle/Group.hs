{-# LANGUAGE OverloadedStrings #-}
module AutoReply.MsgHandle.Group where

import Network.Mirai
import Data.Mirai
import Module.ImageSearch
import Type.Mirai.Update
import AutoReply.HandleEnv
import Data.Monads
import Util.Log
import Data.Maybe
import qualified Data.Text as T (empty)

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

reply' f = do
  upd <- asks update
  conn <- asks connection
  lift $ sendMessage (fromJust $ getChatType upd) conn (f upd)
reply text = reply' (`mkSendMsgT` text)
replyQ text = reply' (`mkSendMsgTQ` text)
replyPicQ text url = reply' (\upd -> mkSendMsgPQ upd text url)