module Util.Time where

import Data.Time ( formatTime, defaultTimeLocale, getZonedTime )

-- format like "%Y-%m-%d", https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime
getDate :: String -> IO String
getDate format = do
  formatTime defaultTimeLocale format <$> getZonedTime