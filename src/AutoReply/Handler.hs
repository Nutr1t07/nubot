{-# LANGUAGE OverloadedStrings #-}
module AutoReply.Handler where

import Data.TaskQueue (TaskQueue, addTask)
import Data.User ( RepliedTable, UserGroup, replaceUser, fetchUser, User (state))
import Network.Mirai ( Connection )
import AutoReply.EventHandle
import Data.Monads ( MonadTrans(lift), ReaderT(runReaderT), ReaderT, MonadTrans(lift), ask, asks )
import Data.Mirai
import Data.Schedule
import Type.Mirai.Update
import AutoReply.MsgHandle.Private ( stateHandler, incStage )
import AutoReply.MsgHandle.Common
import AutoReply.Misc ( equalT, beginWithT )
import AutoReply.HandleEnv
import Data.Maybe ( fromMaybe, fromJust )
import Util.Log (logWT, LogTag (Info), logWT'T)
import Util.Misc (showT)
import Data.IORef

mainHandler :: Int -> TaskQueue -> UserGroup -> TaskListRef -> RepliedTable -> Update -> Connection -> IO ()
mainHandler selfId taskQueue userGrp taskList replyTable upd conn = flip runReaderT env $ do
  lift $ storeMsg upd
  case () of
    _

      | isAddFriendEvent upd ->
          _addFriendHandler

      | isMessage upd ->
          _grpMsgHandler

      | isNewMemberEvent upd ->
          _memberJoinHandler

      | isInvitedToGroupEvent upd ->
          _joinGroupHandler

    --   | isFromUser upd && fromEnum (fromJust $ getUserId upd) /= selfId ->
    --       lift $ addTask taskQueue $ (`runReaderT` env) _privMsgHandler

    --   | isFromUser upd && fromEnum (fromJust $ getUserId upd) /= selfId ->
    --       lift $ addTask taskQueue $ (`runReaderT` env) _privMsgHandler

    -- | isSyncUpdate upd ->
    --     _syncHandler

      | otherwise -> pure ()
  where
    env = HandleEnv conn upd taskQueue replyTable userGrp taskList selfId


_privMsgHandler :: ReaderT HandleEnv IO ()
_privMsgHandler = do
  env <- ask
  upd <- asks update
  lift $ guard' (isMessage upd) $ do
    let (MUpdate updm) = upd
    usr <- fetchUser (userGroup env) (sdr_id $ updm_sender updm)
    logWT'T Info ("[_privMsgHandler] cmd received from "
            <> fromMaybe "UNKNOWN" (getUserRemark upd)
            <> "(" <> showT (state usr) <> ")"
            <> ": " <> fromMaybe "" (getPlainText upd))
    changedUsr <- stateHandler usr (connection env) upd (replyTable env)
    _ <- replaceUser (userGroup env) (incStage 1 changedUsr)
    pure ()

_grpMsgHandler :: ReaderT HandleEnv IO ()
_grpMsgHandler = do
  upd <- asks update
  let msgTxt = fromMaybe "" $ getPlainText upd
      equal' = equalT msgTxt
      begin' = beginWithT msgTxt
  guard' (isMessage upd) $ case () of
      _ | equal' "sp"      -> searchImageHdl
        | equal' "ping"    -> pingHdl

        | equal' "weather" -> getWeatherHdl

        | begin' "baidu"  -> searchBaiduHdl
        | begin' "baike"  -> searchBaikeHdl
        | begin' "google" -> searchGoogleHdl

        | equal' "getSchedule" -> getScheduleHdl
        | begin' "addSchedule" -> addScheduleHdl
        | begin' "rmSchedule"  -> rmScheduleHdl

        | otherwise -> pure ()