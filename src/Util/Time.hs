module Util.Time where
import Data.Time

getDateStr :: IO String
getDateStr = do
  formatTime defaultTimeLocale "%Y-%m-%d" <$> getCurrentTime