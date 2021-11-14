{-# LANGUAGE OverloadedStrings #-}
module AutoReply.MsgHandle where
  
import Data.TaskQueue (TaskQueue, addTask)
import Data.User
import Network.Mirai
import Type.Mirai.Update
import AutoReply.MsgHandle.Private
import AutoReply.MsgHandle.Group
import AutoReply.Misc
import AutoReply.HandleEnv
import Data.Monads
import Data.Mirai
import Data.Maybe

guard' a action = if a then action else pure ()

_privMsgHandler :: ReaderT HandleEnv IO ()
_privMsgHandler = do
  env <- ask
  upd <- asks update
  lift $ guard' (isMessage upd) $ do
    let (MUpdate updm) = upd
    usr <- fetchUser (userGroup env) (sdr_id $ updm_sender updm)
    changedUsr <- stateHandler usr (connection env) upd (replyTable env)
    _ <- replaceUser (userGroup env) (incStage 1 changedUsr) -- stage increase by default
    pure ()

_grpMsgHandler :: ReaderT HandleEnv IO ()
_grpMsgHandler = do
  upd <- asks update
  let msgTxt = fromMaybe "" $ getPlainText upd
      msgTxtEqTo = equalT msgTxt
  guard' (isMessage upd) $ case () of
      _ | msgTxtEqTo "sp" -> searchImageHdl
        | msgTxtEqTo "ping" -> pingHdl
        | otherwise -> pure ()