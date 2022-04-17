{-# LANGUAGE OverloadedStrings #-}
module Util.SystemCall where


import qualified Turtle
import           Turtle                             ( ExitCode (ExitFailure, ExitSuccess) )
import           System.Directory
import           System.Timeout
import           Util.Log
import           Util.Time
import           Util.Misc
import qualified Data.ByteString            as BS
import qualified Data.Text                  as T
import           Data.ByteString.Base64             ( encodeBase64 )
import           Control.Exception
import           Control.Monad
import           Data.Text                          ( Text )

getScreenshot' :: (Int, Int) -> Text -> IO (Maybe Text)
getScreenshot' (width, height) url = do
    logWT'T Info $ "getting screenshot of " <> url
    fileName <- callChromiumScreenshot (width, height) url
    case fileName of
        Nothing -> pure Nothing
        Just name -> do

            code' <- callMogrifyCrop' name
            case code' of
                  ExitFailure _ -> pure Nothing

                  ExitSuccess -> do
                    picContent <- catch ( encodeBase64 <$> BS.readFile name) (\x -> logWT Error ("error reading picture base64" <> show (x::SomeException)) >> pure "")
                    case picContent of
                      "" -> pure Nothing
                      x  -> pure $ Just x

callMogrifyCrop' :: String -> IO ExitCode
callMogrifyCrop' name = do
  Turtle.proc "mogrify" args Turtle.empty
  where
      args = [ "-colors", "255"
             , "-resize", "80%"
             , T.pack name]

getScreenshot :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Text -> IO (Maybe Text)
getScreenshot ((cropWidth, cropHeight), (x, y)) (width, height) url = fmap join $ timeout 60000000 $ do
    logWT'T Info $ "getting screenshot of " <> url
    fileName <- callChromiumScreenshot (width, height) url
    case fileName of
        Nothing -> pure Nothing
        Just name -> do

            code' <- callMogrifyCrop (cropWidth,cropHeight) (x,y) name
            case code' of
                  ExitFailure _ -> pure Nothing

                  ExitSuccess -> do
                    picContent <- catch ( encodeBase64 <$> BS.readFile name) (\x -> logWT Error ("error reading picture base64" <> show (x::SomeException)) >> pure "")
                    case picContent of
                      "" -> pure Nothing
                      x  -> pure $ Just x


callMogrifyCrop :: (Int, Int) -> (Int, Int) -> String -> IO ExitCode
callMogrifyCrop (width, height) (x, y) name = do
  Turtle.proc "mogrify" args Turtle.empty
  where
      args = [ "-crop"
             , showT width <> "x" <> showT height <> "+" <> showT x <> "+" <> showT y
             , "-fuzz", "1%"
             , "-trim"
             , T.pack name]

callChromiumScreenshot :: (Int, Int) -> Text -> IO (Maybe FilePath)
callChromiumScreenshot (width, height) url = do
  ret <- Turtle.proc "chromium" args Turtle.empty
  case ret of
    ExitFailure _ -> pure Nothing
    ExitSuccess   -> do
      fileName <- ("cache/" <>) . (<> ".png") . show <$> getPOSIXSec
      copyFile "screenshot.png" fileName
      removeFile "screenshot.png"
      exist <- doesFileExist fileName
      if exist then pure (Just fileName) else pure Nothing
  where
      args = [ "--headless"
             , "--disable-gpu"
             , "--no-sandbox"
             , "--screenshot"
             , "--hide-scrollbars"
             , "--ignore-certificate-errors-spki-list"
             , "--virtual-time-budget=10000"
             , "--timeout=60000"
             , "--window-size=" <> showT width <> "," <> showT height
             , url]
