module AutoReply.Handler where
  
import Data.TaskQueue (TaskQueue, addTask)
import Data.User ( RepliedTable, UserGroup )
import Network.Mirai ( Connection )
import Type.Mirai.Update ( Update )
import AutoReply.MsgHandle ( _grpMsgHandler, _privMsgHandler )
import AutoReply.EventHandle
    ( _addFriendHandler, _joinGroupHandler, _memberJoinHandler )
import AutoReply.HandleEnv (HandleEnv(HandleEnv))
import Data.Monads ( MonadTrans(lift), ReaderT(runReaderT) )
import Data.Mirai
    ( getUserId,
      isAddFriendEvent,
      isFromGroup,
      isFromUser,
      isInvitedToGroupEvent,
      isNewMemberEvent,
      storeMsg )
import Data.Maybe ( fromJust )
import Data.Schedule (Schedule)

mainHandler :: Int -> TaskQueue -> UserGroup -> Schedule -> RepliedTable -> Update -> Connection -> IO ()
mainHandler selfId taskQueue userGrp schedule replyTable upd conn = flip runReaderT env $ do
  lift $ storeMsg upd
  case () of
    _ 
    --   | isFromUser upd && fromEnum (fromJust $ getUserId upd) /= selfId ->
    --       lift $ addTask taskQueue $ (`runReaderT` env) _privMsgHandler

    --   | isFromUser upd && fromEnum (fromJust $ getUserId upd) /= selfId ->
    --       lift $ addTask taskQueue $ (`runReaderT` env) _privMsgHandler

    --   | isAddFriendEvent upd ->
    --       _addFriendHandler

      | isFromGroup upd ->
          _grpMsgHandler
      
      | isNewMemberEvent upd -> 
          _memberJoinHandler
      
      | isInvitedToGroupEvent upd -> 
          _joinGroupHandler

      | otherwise -> pure ()
  where
    env = HandleEnv conn upd taskQueue replyTable userGrp schedule selfId