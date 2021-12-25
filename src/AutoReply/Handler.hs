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
import Data.Schedule (Schedule)

mainHandler :: Int -> TaskQueue -> UserGroup -> Schedule -> RepliedTable -> Update -> Connection -> IO ()
mainHandler selfId taskQueue userGrp schedule replyTable upd conn = flip runReaderT env $ do
  lift $ storeMsg upd
  case () of
    _ | isFromUser upd && fromEnum (fromJust $ getUserId upd) /= selfId ->
          lift $ addTask taskQueue $ (`runReaderT` env) _privMsgHandler

      | isFromUser upd && fromEnum (fromJust $ getUserId upd) /= selfId ->
          lift $ addTask taskQueue $ (`runReaderT` env) _privMsgHandler

      | isAddFriendEvent upd ->
          _addFriendHandler

      | isFromGroup upd ->
          _grpMsgHandler
      
      | isNewMemberEvent upd -> 
          _memberJoinHandler
      
      | isInvitedToGroupEvent upd -> 
          _joinGroupHandler

      | otherwise -> pure ()
  where
    env = HandleEnv conn upd taskQueue replyTable userGrp schedule selfId