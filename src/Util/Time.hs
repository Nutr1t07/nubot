module Util.Time where

import Data.Time ( formatTime, defaultTimeLocale, getZonedTime, zonedTimeToLocalTime )
import Data.Time.Clock ( nominalDay )
import Data.Time.LocalTime ( addLocalTime )

-- format like "%Y-%m-%d", https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime
getDate :: String -> IO String
getDate format = do
  formatTime defaultTimeLocale format <$> getZonedTime

getTomorrowDate :: String -> IO String
getTomorrowDate format = do
  formatTime defaultTimeLocale format . addLocalTime nominalDay . zonedTimeToLocalTime <$> getZonedTime

