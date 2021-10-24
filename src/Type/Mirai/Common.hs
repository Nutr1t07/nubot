{-# LANGUAGE DeriveGeneric #-}
module Type.Mirai.Common where

import Data.Text ( Text )
import Data.Aeson ( FromJSON(parseJSON), ToJSON(toJSON) )
import GHC.Generics ( Generic )
import Util.Json (dropParseJSON, dropToJSON)
import Codec.Serialise.Class (Serialise)

data ChatType = Friend | Group | Temp deriving (Show)

emptyChainMessage :: Text -> ChainMessage
emptyChainMessage t = ChainMessage t Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data ChainMessage = ChainMessage {
    cm_type     :: Text
    
  -- source, file
  , cm_id       :: Maybe Integer

  -- source
  , cm_time     :: Maybe Integer 

  -- plain
  , cm_text     :: Maybe Text

  -- image
  , cm_imageId  :: Maybe Text
  , cm_url      :: Maybe Text

  -- miraicode
  , cm_code        :: Maybe Text

  -- quote
  , cm_origin   :: Maybe [ChainMessage]

  -- at
  , cm_target   :: Maybe Integer
  , cm_display  :: Maybe Text

} deriving (Show, Generic)
instance FromJSON ChainMessage where
  parseJSON = dropParseJSON 3
instance ToJSON ChainMessage where
  toJSON = dropToJSON 3
instance Serialise ChainMessage