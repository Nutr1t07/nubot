{-# LANGUAGE DeriveGeneric #-}
module Type.Mirai.Request where

import Type.Mirai.Common ( ChainMessage )
import GHC.Generics ( Generic )
import Data.Aeson (ToJSON (toJSON))
import Util.Json (dropToJSON)
import Data.Text (Text)



data RequestContent = RSendMsg SendMsg | REvent Event
 deriving (Generic, Show)
instance ToJSON RequestContent where
  toJSON = dropToJSON 0

data SendMsg = SendMsg {
    sm_qq     :: Maybe Integer
  , sm_group  :: Maybe Integer
  , sm_quote  :: Maybe Integer
  , sm_messageChain :: [ChainMessage]
} deriving (Generic, Show)
instance ToJSON SendMsg where
  toJSON = dropToJSON 3

data Event = Event {
    ev_eventId   :: Integer
  , ev_fromId    :: Integer
  , ev_groupId   :: Maybe Integer
  , ev_operate   :: Int
  , ev_message   :: Text
} deriving (Generic, Show)
instance ToJSON Event where
  toJSON = dropToJSON 3