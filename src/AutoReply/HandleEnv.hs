module AutoReply.HandleEnv where


import Data.TaskQueue (TaskQueue)
import Data.User
import Network.Mirai
import Type.Mirai.Update
import Data.Schedule (Schedule)

data HandleEnv = HandleEnv {
    connection :: Connection
  , update  :: Update
  , taskQueue :: TaskQueue
  , replyTable :: RepliedTable
  , userGroup :: UserGroup
  , schedule :: Schedule
  , selfId :: Int
}