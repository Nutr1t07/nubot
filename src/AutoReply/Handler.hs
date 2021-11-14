module AutoReply.Handler where
  
import Data.TaskQueue (TaskQueue, addTask)
import Data.User
import Network.Mirai
import Type.Mirai.Update
import AutoReply.MsgHandle
import AutoReply.EventHandle
import AutoReply.HandleEnv (HandleEnv(HandleEnv))
import Data.Monads
import Data.Mirai
import Data.Maybe
import Control.Monad

mainHandler :: Int -> TaskQueue -> UserGroup -> RepliedTable -> Update -> Connection -> IO ()
mainHandler selfId taskQueue userGrp replyTable upd conn = (flip runReaderT) env $ do
  lift $ storeMsg upd
  when (isFromUser upd && fromEnum (fromJust $ getUserId upd) /= selfId) $
    lift $ addTask taskQueue $
      (`runReaderT` env) privMsgHandler
  when (isAddFriendEvent upd) $ addFriendHandler 
  when (isFromGroup upd) $ grpMsgHandler
  where
    env = HandleEnv conn upd taskQueue replyTable userGrp selfId

addFriendHandler :: ReaderT HandleEnv IO ()
addFriendHandler = _addFriendHandler

grpMsgHandler :: ReaderT HandleEnv IO ()
grpMsgHandler = _grpMsgHandler

privMsgHandler :: ReaderT HandleEnv IO ()
privMsgHandler = _privMsgHandler