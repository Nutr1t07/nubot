{-# LANGUAGE OverloadedStrings #-}
module Module.Weather where

import           Turtle
import           Util.Misc                     ( showT, searchBetweenBL, searchAllBetweenBL, searchAllBetweenBL', isSubblOf, transBL2TS )
import           Network.Wreq       as Wreq
import           Control.Lens                  ( (^.) )
import qualified Data.Text          as T
import           Data.Text                     ( Text )
import           Data.Text.Encoding as T
import           Data.List                     ( findIndices, intersperse )


getNextRainyDay :: IO (Maybe Text)
getNextRainyDay = do
    raw <- (^. responseBody) <$> Wreq.get "https://weather.com/weather/tenday/l/23.285665,116.148000"
    let
        weatherTypes = drop 1 $ take 7 $ searchAllBetweenBL' ">" "</span></div><div data-testid=\"P" raw
        weatherDates = drop 1 $ take 7 $ searchAllBetweenBL' ">" "</h2><d" raw
        rainPercents = drop 1 $ take 7 $ searchAllBetweenBL "PercentageValue\">" "<" raw
    let
        indices = findIndices ("Shower" `isSubblOf`) weatherTypes

    case indices of
        [] -> pure Nothing
        _  -> pure . Just $ (T.pack "未来降水可能出现于: \n") <>
                T.unlines ((\i -> showT i <> "天后(" <> transBL2TS (weatherDates !! i) <> ") (" <> transBL2TS (rainPercents !! i) <> ")") <$> indices)

write7DayScreenshot :: IO Bool
write7DayScreenshot = do
    code <- callChromiumScreenshot (1280, 1700) "https://weather.com/zh-CN/weather/tenday/l/23.285665,116.148000"
    case code of
        ExitFailure _ -> pure False
        _ -> do
            code' <- callMogrifyCrop (770,430) (73,585)
            case code' of
                  ExitSuccess -> pure True
                  _ -> pure False

callMogrifyCrop :: (Int, Int) -> (Int, Int) -> IO ExitCode
callMogrifyCrop (width, height) (x, y) = 
  Turtle.proc "mogrify" args empty
  where 
      args = [ "-crop"
             , showT width <> "x" <> showT height <> "+" <> showT x <> "+" <> showT y
             , "screenshot.png"]

callChromiumScreenshot :: (Int, Int) -> Text -> IO ExitCode
callChromiumScreenshot (width, height) url =
  Turtle.proc "chromium" args empty
  where 
      args = [ "--headless"
             , "--disable-gpu"
             , "--no-sandbox"
             , "--screenshot"
             , "--window-size=" <> showT width <> "," <> showT height
             , url]
