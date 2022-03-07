{-# LANGUAGE OverloadedStrings #-}
module AutoReply.EventHandle where

import Data.TaskQueue (TaskQueue, addTask)
import Data.User
    ( fetchUser, replaceUser, State(Idle), User(userId) )
import Network.Mirai ( sendCommand, sendMessage )
import Type.Mirai.Update
    ( EventUpdate(upde_fromId, upde_nick, upde_message), Update(EUpdate) )
import AutoReply.MsgHandle.Private ( setState )
import AutoReply.HandleEnv
    ( HandleEnv(userGroup, update, connection) )
import Data.Monads ( ask, asks, MonadTrans(lift), ReaderT )
import Control.Monad ( void )
import Data.Mirai
    ( getGroupId,
      getUserId,
      isEvent,
      mkFriendEventResp,
      transUpd2SendMsgT,
      mkGroupEventResp )
import Data.Maybe ( fromJust, fromMaybe )
import Type.Mirai.Common ( ChatType(Group, Friend) )
import Util.Log ( logWT, LogTag(Info) )
import Util.Misc ( showT )

guard' :: Applicative f => Bool -> f () -> f ()
guard' a action = if a then action else pure ()

_addFriendHandler :: ReaderT HandleEnv IO ()
_addFriendHandler = do
  env <- ask
  upd <- asks update
  lift $ guard' (isEvent upd) $ do
    let reply txt = sendMessage Friend (connection env) (transUpd2SendMsgT upd txt)
    usr <- fetchUser (userGroup env) (fromJust $ getUserId upd)
    let resp = mkFriendEventResp upd 0
    logWT Info ("[_addFriendHandler] new friend " <> show (userId usr) <> " added")
    sendCommand "resp_newFriendRequestEvent" (connection env) resp
    reply "您的好友请求已由机器人自动同意"
    void $ replaceUser (userGroup env) (setState Idle usr)

_joinGroupHandler :: ReaderT HandleEnv IO ()
_joinGroupHandler = do
  env <- ask
  upd <- asks update
  lift $ guard' (isEvent upd) $ do
    let (EUpdate upde) = upd
    let reply txt = sendMessage Group (connection env) (transUpd2SendMsgT upd txt)
    let resp = mkGroupEventResp upd 0
    logWT Info ("[_joinGroupHandler] being invited to group"
        <> show (getGroupId upd))
    sendCommand "resp_botInvitedJoinGroupRequestEvent" (connection env) resp
    reply $ "事件ID" <> showT (upde_fromId upde)
      <> "已受理，邀请人:" <> showT (getUserId upd)
    pure ()

_memberJoinHandler :: ReaderT HandleEnv IO ()
_memberJoinHandler = do
  env <- ask
  upd <- asks update
  lift $ guard' (isEvent upd) $ do
    let (EUpdate upde) = upd
    let reply txt = sendMessage Group (connection env) (transUpd2SendMsgT upd txt)
    let resp = mkGroupEventResp upd 0
    logWT Info ("[_memberJoinHandler] new member requests to join a group"
      <> show (getGroupId upd))
    let id'' = fromMaybe 0 $ upde_fromId upde
        nick'' = fromMaybe "null" $ upde_nick upde
        msg'' = fromMaybe "null" $ upde_message upde
    reply $ "一位用户正在请求加入本群: \n" <> nick'' <>" (" <> showT id'' <> ")"
      <> "\n申请消息:\n\"" <> msg'' <> "\""
    pure ()