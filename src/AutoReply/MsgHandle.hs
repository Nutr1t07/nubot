{-# LANGUAGE OverloadedStrings #-}
module AutoReply.MsgHandle where
  
import Data.TaskQueue (TaskQueue, addTask)
import Data.User ( replaceUser, fetchUser )
import Type.Mirai.Update
    ( Sender(sdr_id), MessageUpdate(updm_sender), Update(MUpdate) )
import AutoReply.MsgHandle.Private ( stateHandler, incStage )
import AutoReply.MsgHandle.Group
    ( searchImageHdl, pingHdl, searchBaiduHdl, addScheduleHdl, getScheduleHdl, rmScheduleHdl )
import AutoReply.Misc ( equalT, beginWithT )
import AutoReply.HandleEnv
    ( HandleEnv(connection, replyTable, userGroup, update) )
import Data.Monads ( ReaderT, MonadTrans(lift), ask, asks )
import Data.Mirai ( getPlainText, isMessage )
import Data.Maybe ( fromMaybe )
import Util.Log (logWT, LogTag (Info), logWT'T)

guard' :: Applicative f => Bool -> f () -> f ()
guard' a action = if a then action else pure ()

_privMsgHandler :: ReaderT HandleEnv IO ()
_privMsgHandler = do
  env <- ask
  upd <- asks update
  lift $ logWT'T Info ("[_privMsgHandler] cmd received: " 
            <> fromMaybe "" (getPlainText upd))
  lift $ guard' (isMessage upd) $ do
    let (MUpdate updm) = upd
    usr <- fetchUser (userGroup env) (sdr_id $ updm_sender updm)
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
      _ | equal' "sp"     -> searchImageHdl
        | equal' "ping"   -> pingHdl
        | begin' "baidu'" -> searchBaiduHdl
        | begin' "addSchedule'" -> addScheduleHdl
        | begin' "getSchedule'" -> getScheduleHdl
        | begin' "rmSchedule'" -> rmScheduleHdl
        | otherwise -> pure ()