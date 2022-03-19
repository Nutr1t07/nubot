{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Module.Holiday ( getHolidayText_out ) where

import           Data.Text (Text)
import           Data.Aeson ( FromJSON(parseJSON), eitherDecode, ToJSON(toJSON) )
import           Network.Wreq as Wreq ( getWith, defaults, param, responseBody )
import           Control.Lens ( (&), (^.), (.~) )
import qualified Data.Text as T
import           Util.Log (logWT, LogTag (Error))
import           Util.Json ( dropParseJSON, dropToJSON )
import           GHC.Generics ( Generic )
import           Util.Time ( getTomorrowDate )
import           Type.Mirai.Common  ( ChainMessage(ChainMessage) )
import           Data.Mirai  ( mkMessageChainT )

getHolidayText_out :: IO [[ChainMessage]]
getHolidayText_out = do
  rst <- getHolidayText
  case rst of
    Nothing -> pure []
    Just x -> pure $ [mkMessageChainT x]

getHolidayText :: IO (Maybe Text)
getHolidayText = do
  date <- getTomorrowDate "%Y-%m-%d"
  hld' <- getHoliday date
  let textIt hld = case hld_name hld of
                      "" -> Nothing
                      _  -> do
                        let number = if hld_now hld /= 0 then "的第" <> digitToCN (hld_now hld) <> "天" else ""
                        let name = hld_name hld <> if hld_enname hld /= "" then " (" <> hld_enname hld <> ") " else ""
                        let tip  = if hld_now hld == 0 then hld_tip hld else ""
                        Just $ "明天是" <> name <> number <> "。" <> tip
  pure $ textIt =<< hld'
  where
    digitToCN 0 = "零"
    digitToCN 1 = "一"
    digitToCN 2 = "二"
    digitToCN 3 = "三"
    digitToCN 4 = "四"
    digitToCN 5 = "五"
    digitToCN 6 = "六"
    digitToCN 7 = "七"
    digitToCN 8 = "八"
    digitToCN 9 = "九"
    digitToCN _ = "异"

getHoliday :: String -> IO (Maybe Holiday)
getHoliday date = do
  r <- Wreq.getWith opts  "http://api.tianapi.com/jiejiari/index"
  let rst = eitherDecode (r ^. responseBody)
  case rst of
    Left err -> logWT Error ("parsing holiday failed:" <> err) >> pure Nothing
    Right rsts -> case rst_newslist rsts of
      [] -> logWT Error "empty holiday list" >> pure Nothing
      x:_ -> pure $ Just x
  where
    opts =
      defaults
        &  param "key"
        .~ ["5adb2193919b021956327efec2d3f9ef"]
        &  param "date"
        .~ [T.pack date]
        &  param "mode"
        .~ ["1"]
        &  param "type"
        .~ ["0"]

data HolidayRequestResult = HolidayRequestResult {
    rst_code :: Int
  , rst_msg      :: String
  , rst_newslist :: [Holiday]
} deriving (Show, Generic)
instance FromJSON HolidayRequestResult where
  parseJSON = dropParseJSON 4
instance ToJSON HolidayRequestResult where
  toJSON = dropToJSON 4

data Holiday = Holiday {
    hld_date       :: String
  , hld_lunarmonth :: Text
  , hld_lunarday   :: Text
  , hld_name       :: Text
  , hld_enname     :: Text
  , hld_now        :: Int
  , hld_tip        :: Text
  , hld_rest       :: Text
} deriving (Show, Generic)
instance FromJSON Holiday where
  parseJSON = dropParseJSON 4
instance ToJSON Holiday where
  toJSON = dropToJSON 4