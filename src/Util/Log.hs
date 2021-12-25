{-# LANGUAGE OverloadedStrings #-}
module Util.Log ( logErr, logWT, logWT'C8, logWT'T, logWT'BL, LogTag(..))where

import Data.Time ( defaultTimeLocale, formatTime, getZonedTime )
import System.IO ( hFlush, stdout )
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as Char8L
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (log)
import qualified Data.Text.IO as T

enviroment :: String
-- enviroment = "Debug"
enviroment = "Release"

data LogTag = Info | Warning | Error | Debug
  deriving (Show, Eq)

log :: Semigroup t => (t -> IO a) -> ([Char] -> t) -> LogTag -> t -> IO ()
log printFunc packFunc tag msg
  | tag == Debug && enviroment /= "Debug" = pure ()
  | otherwise = do
      info <- packFunc <$> mkInfo tag
      printFunc $ info <> msg
      hFlush stdout
  where
    mkInfo logTag = do
      t <- formatTime defaultTimeLocale "%Y/%m/%d %H:%M" <$> getZonedTime
      pure $ concat ["[", t, "] ", "[", show logTag, "] "]

-- Log with tag
logWT :: LogTag -> String -> IO ()
logWT = log putStrLn id

logWT'C8 :: LogTag -> B.ByteString -> IO ()
logWT'C8 = log Char8.putStrLn Char8.pack

logWT'BL :: LogTag -> BL.ByteString -> IO ()
logWT'BL = log Char8L.putStrLn (BL.fromStrict . Char8.pack)

logWT'T:: LogTag -> Text -> IO ()
logWT'T = log T.putStrLn T.pack

-- | Log errors with action name and details.
logErr :: String -> String -> IO ()
logErr msg errText = logWT Error (msg ++ ": " ++ errText)