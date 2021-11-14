module AutoReply.HandleEnv where


import Data.TaskQueue (TaskQueue)
import Data.User
import Network.Mirai
import Type.Mirai.Update

data HandleEnv = HandleEnv {
    connection :: Connection
  , update  :: Update
  , taskQueue :: TaskQueue
  , replyTable :: RepliedTable
  , userGroup :: UserGroup
  , selfId :: Int
}