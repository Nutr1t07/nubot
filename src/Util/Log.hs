{-# LANGUAGE OverloadedStrings #-}
module Util.Log ( logErr, logWT, logWT'C8, logWT'T, logWT'BL, LogTag(..))where

import           Data.Time ( defaultTimeLocale, formatTime, getZonedTime )
import           System.IO ( hFlush, stdout )
import           Data.Text (Text)
import           Data.String ( IsString )
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as Char8L
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Prelude hiding (log)







enviroment :: String
-- enviroment = "Debug"
enviroment = "Release"

data LogTag = Info | Warning | Error | Debug
  deriving (Show, Eq)

log :: (Semigroup t, IsString t, Show t) => (t -> IO a) -> (FilePath -> t -> IO ()) -> ([Char] -> t) -> LogTag -> t -> IO ()
log printFunc appendFunc packFunc tag msg
  | tag == Debug && enviroment /= "Debug" = pure ()
  | otherwise = do
      info <- packFunc <$> mkInfo tag
      let logText = info <> msg
      printFunc logText
      hFlush stdout
      appendFunc "log.txt" (logText <> "\n")
  where
    mkInfo logTag = do
      t <- formatTime defaultTimeLocale "%Y/%m/%d %H:%M" <$> getZonedTime
      pure $ concat ["[", t, "] ", "[", show logTag, "] "]

-- Log with tag
logWT :: LogTag -> String -> IO ()
logWT = log putStrLn appendFile id

logWT'C8 :: LogTag -> B.ByteString -> IO ()
logWT'C8 = log Char8.putStrLn Char8.appendFile Char8.pack

logWT'BL :: LogTag -> BL.ByteString -> IO ()
logWT'BL = log Char8L.putStrLn Char8L.appendFile (BL.fromStrict . Char8.pack)

logWT'T:: LogTag -> Text -> IO ()
logWT'T = log T.putStrLn T.appendFile T.pack

-- | Log errors with action name and details.
logErr :: String -> String -> IO ()
logErr msg errText = logWT Error (msg ++ ": " ++ errText)