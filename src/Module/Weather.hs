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
import           Data.Char                     ( ord )

import           Util.Log

getNextRainyDay :: IO (Maybe Text)
getNextRainyDay = do
    raw <- (^. responseBody) <$> Wreq.get "https://weather.com/zh-CN/weather/tenday/l/23.285665,116.148000"
    let
        -- it's actually a 15-day forecast. and we only need 7 days.
        weatherTypes = drop 1 $ take 15 $ searchAllBetweenBL' ">" "</span></div><div data-testid=\"P" raw
        weatherDates = fmap transBL2TS $ drop 1 $ take 15 $ searchAllBetweenBL' ">" "</h2><d" raw
        
        rainPercents' = take 30 $ searchAllBetweenBL' ">" "</span></div><div c" raw
        rainPercents  = transBL2TS <$> if length rainPercents' == 29
                           then drop 1 rainPercents'
                           else drop 2 rainPercents'
    let
        rainPcIndices = findIndices (\x -> T.length x == 4      -- '100%' has 4 characters
                                       || (T.length x == 3 && ((ord.T.head) x) > 50) -- any larger than '30%'
                                    ) rainPercents

    case rainPcIndices of
        [] -> pure Nothing
        _  -> pure . Just $ (T.pack "未来降水可能出现于: \n") <>
                T.unlines (fmap (\i -> 
                    weatherDates !! (div i 2)
                    <> ", "
                    <> (if mod i 2 == 0 then "日" else "夜")
                    <> ", "
                    <> (rainPercents !! i)
                    ) rainPcIndices)


write7DayScreenshot :: IO Bool
write7DayScreenshot = do
    code <- callChromiumScreenshot (1280, 1700) "https://weather.com/zh-CN/weather/tenday/l/23.285665,116.148000"
    case code of
        ExitFailure err -> pure False
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
