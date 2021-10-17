{-# LANGUAGE DeriveGeneric #-}
module Util.Config where

import Data.Text ( Text )
import Data.Aeson ( FromJSON )
import GHC.Generics ( Generic )

data Config = Config {
    ws_port :: Int
  , ws_addr :: String
  , mirai_verify_key :: String
  , mirai_qq_id :: Int
} deriving(Generic)
instance FromJSON Config