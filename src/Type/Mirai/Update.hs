{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Mirai.Update where

import Data.Text ( Text )
import Data.Aeson ( FromJSON(parseJSON), Object )
import GHC.Generics ( Generic )
import Util.Json (dropParseJSON)
import Type.Mirai.Common ( ChainMessage )
import Data.Aeson.Types ( FromJSON(parseJSON) )
import Codec.Serialise.Class (Serialise)

data WebsocketWrapper = WebsocketWrapper {
    wrap_syncId :: Text
  , wrap_data   :: Update
} deriving (Show, Generic)
instance FromJSON WebsocketWrapper where
  parseJSON = dropParseJSON 5

data Update = MUpdate MessageUpdate | EUpdate EventUpdate | SUpdate SyncUpdate 
  deriving (Show, Generic)
instance Serialise Update
instance FromJSON Update where
  parseJSON = dropParseJSON 0

data SyncUpdate = SyncUpdate {
    upds_code         :: Int
  , upds_message      :: Maybe Text
  , udds_session      :: Maybe Text
} deriving (Show, Generic)
instance Serialise SyncUpdate
instance FromJSON SyncUpdate where
  parseJSON = dropParseJSON 5

data MessageUpdate = MessageUpdate {
    updm_type         :: Text
  , updm_sender       :: Sender
  , updm_messageChain :: [ChainMessage]
} deriving (Show, Generic)
instance Serialise MessageUpdate
instance FromJSON MessageUpdate where
  parseJSON = dropParseJSON 5

data EventUpdate = EventUpdate {
    upde_type         :: Text
  , upde_eventId      :: Maybe Integer
  , upde_fromId       :: Maybe Integer
  , upde_groupId      :: Maybe Integer
} deriving (Show, Generic)
instance Serialise EventUpdate
instance FromJSON EventUpdate where
  parseJSON = dropParseJSON 5

data Sender = Sender {
    sdr_id :: Integer
    
    -- friend or stranger
  , sdr_nickname :: Maybe Text
  , sdr_remark   :: Maybe Text

    -- group or temp
  , sdr_memberName :: Maybe Text
  , sdr_group      :: Maybe SdrGroup
} deriving (Show, Generic)
instance FromJSON Sender where
  parseJSON = dropParseJSON 4
instance Serialise Sender

data SdrGroup = SdrGroup {
    grp_id :: Integer
  , grp_name :: Text
} deriving (Show, Generic)
instance FromJSON SdrGroup where
  parseJSON = dropParseJSON 4
instance Serialise SdrGroup
