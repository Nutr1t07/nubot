{-# LANGUAGE OverloadedStrings #-}
module AutoReply.EventHandle where

import Data.TaskQueue (TaskQueue, addTask)
import Data.User
import Network.Mirai
import Type.Mirai.Update
import AutoReply.MsgHandle.Private
import AutoReply.MsgHandle.Group
import AutoReply.HandleEnv
import Data.Monads
import Control.Monad
import Data.Mirai
import Data.Maybe
import Type.Mirai.Common
import Util.Log

guard' a action = if a then action else pure ()

_addFriendHandler :: ReaderT HandleEnv IO ()
_addFriendHandler = do
  env <- ask
  upd <- asks update
  lift $ guard' (isEvent upd) $ do
    let reply txt = sendMessage Friend (connection env) (mkSendMsgT upd txt)
    usr <- fetchUser (userGroup env) (fromJust $ getUserId upd)
    let resp = mkFriendEventResp upd 0
    logWT Info ("new friend " <> show (userId usr) <> " added")
    sendCommand "resp_newFriendRequestEvent" (connection env) resp
    reply "嗨，我当前不在线"
    reply "您的好友请求已由机器人自动同意，发送『帮助』以查看此机器人的功能"
    void $ replaceUser (userGroup env) (setState Idle usr)