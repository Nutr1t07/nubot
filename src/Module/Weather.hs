{-# LANGUAGE OverloadedStrings #-}
module Module.Weather where

import qualified Turtle
import           Turtle                        ( ExitCode (ExitFailure, ExitSuccess) )
import           Util.Misc                     ( showT, searchBetweenBL, searchAllBetweenBL, searchAllBetweenBL', isSubblOf, transBL2TS )
import           Network.Wreq       as Wreq
import           Control.Lens                  ( (^.) )
import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import           Data.Text                     ( Text )
import           Data.Text.Encoding as T
import           Data.List          as L       ( findIndices, intersperse )
import           Data.Char                     ( ord )
import           System.Directory
import           Control.Exception
import           Util.Log
import           Util.Time
import           Util.SystemCall

import           Type.Mirai.Common  ( ChainMessage(ChainMessage) )
import           Data.Mirai  ( mkMessageChainT )

getRainyDay_out :: IO [[ChainMessage]]
getRainyDay_out = do
  rst <- getNextRainyDay
  case rst of
    Nothing -> pure []
    Just x -> pure $ [mkMessageChainT x]

getNextRainyDay :: IO (Maybe Text)
getNextRainyDay = do
    raw <- (^. responseBody) <$> Wreq.get "https://weather.com/zh-CN/weather/tenday/l/23.285665,116.148000"
    let
        -- it's actually a 15-day forecast. and we only need 7 days.
        weatherTypes = drop 1 $ take 8 $ searchAllBetweenBL' ">" "</span></div><div data-testid=\"P" raw
        weatherDates = fmap transBL2TS $ drop 1 $ take 8 $ searchAllBetweenBL' ">" "</h2><d" raw

        rainPercents' = take 30 $ searchAllBetweenBL' ">" "</span></div><div c" raw
        rainPercents  = take 14 $ transBL2TS <$> if length rainPercents' == 29
                           then drop 1 rainPercents'
                           else drop 2 rainPercents'
    let
        rainPcIndices = findIndices (\x -> T.length x == 4      -- '100%' has 4 characters
                                       || (T.length x == 3 && ((ord.T.head) x) > 50) -- any larger than '30%'
                                    ) rainPercents
        weekInfo = foldr (zipWith (+)) [0,0,0,0,0,0,0]
         (fmap (\i ->
          let weekday = T.takeWhile (/= ' ') $ weatherDates !! (div i 2)
          in  weekday2Digit weekday) rainPcIndices)
        displayInfo xs = mconcat $ L.intersperse " " $ fmap (\x -> case x of
          1 -> "溦"
          2 -> "霈"
          0 -> "无"
          5 -> "今"
          8 -> "〇"
          _ -> "？") xs
        today = let raw = weekday2Digit $ T.takeWhile (/= ' ') $ head weatherDates
                    real = length $ takeWhile (==0) $ (drop 1 raw) <> (take 1 raw) in
                real
        breaked = (,)
              (replicate (today) 8 <> [5] <> drop (today+1) weekInfo)
              (take (today+1) weekInfo <> replicate (6-today) 8)
        rainDays = length $ filter (/=0) weekInfo

    case rainPcIndices of
        [] -> pure Nothing
        _  -> pure . Just $ "未来七天内有" <> showT rainDays <> "天可能出现降水: \n" <>
                  "--------------------\n" <>
                  "日 一 二 三 四 五 六\n" <>
                  displayInfo (fst breaked) <> "\n" <>
                  displayInfo (snd breaked) <> "\n" <>
                  "--------------------\n" <>
                  mconcat (intersperse "\n" (fmap (\i ->
                    weatherDates !! (div i 2)
                    <> ", "
                    <> (if mod i 2 == 0 then "日" else "夜")
                    <> ", "
                    <> (rainPercents !! i)
                    ) rainPcIndices))
    where
      weekday2Digit x = case x of
        "周日" -> [1,0,0,0,0,0,0]
        "周一" -> [0,1,0,0,0,0,0]
        "周二" -> [0,0,1,0,0,0,0]
        "周三" -> [0,0,0,1,0,0,0]
        "周四" -> [0,0,0,0,1,0,0]
        "周五" -> [0,0,0,0,0,1,0]
        "周六" -> [0,0,0,0,0,0,1]
        _ -> [0,0,0,0,0,0,0]


get7DayScreenshot :: IO (Maybe Text)
get7DayScreenshot = getScreenshot ((770,430), (73,585)) (1280, 1700) "https://weather.com/zh-CN/weather/tenday/l/23.285665,116.148000"