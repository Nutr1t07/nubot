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
import           Data.Maybe
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

getNextRainyHour :: IO [Int]
getNextRainyHour = do
    raw <- (^. responseBody) <$> Wreq.get "https://weather.com/zh-CN/weather/hourbyhour/l/23.285665,116.148000"
    let
        nextDayRaw = (!! 1) $ searchAllBetweenBL "id=\"currentDateId" "id=\"currentDateId" raw
        rainPercents = transBL2TS <$> searchAllBetweenBL "data-testid=\"PercentageValue\">" "<" nextDayRaw
        rainyHours = findIndices (\x -> T.length x == 4      -- '100%' has 4 characters
                                    || (T.length x == 3 && ((ord.T.head) x) > 50) -- any larger than '30%'
                                 ) rainPercents
    pure rainyHours

hoursToText :: [Int] -> Maybe Text
hoursToText [] = Nothing
hoursToText xss = Just $ go (-1) False xss
  where go :: Int -> Bool -> [Int] -> Text
        go last False (x:y:xs) = if x+1 == y then go x True (y:xs) else showT x <> "、" <> (go y False (y:xs))
        go last True (x:y:xs) = if x+1 == y then go last True (y:xs) else showT last <> "~" <> showT x <> "、" <> (go y False (y:xs))
        go last False (x:[]) = showT x
        go last True (x:[]) = showT last <> "~" <> showT x

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
        rainPcIndices = findIndices (\x -> T.length x == 4      -- '100%' has 4 characters
                                       || (T.length x == 3 && ((ord.T.head) x) > 50) -- any larger than '30%'
                                    ) rainPercents
        weekInfo = foldr (zipWith (+)) [0,0,0,0,0,0,0]
         (fmap (\i ->
          let weekday = T.takeWhile (/= ' ') $ weatherDates !! (div i 2)
          in  weekday2Digit (mod i 2) weekday) rainPcIndices)
        displayInfo xs = mconcat $ L.intersperse " " $ fmap (\x -> case x of
          1 -> "日"
          2 -> "夜"
          3 -> "霈"
          0 -> "无"
          5 -> "今"
          8 -> "〇"
          _ -> "？") xs
        today = let raw = weekday2Digit 0 $ T.takeWhile (/= ' ') $ head weatherDates
                in length $ takeWhile (==0) $ (drop 1 raw) <> (take 1 raw)
        breaked = (,)
              (replicate (today) 8 <> [5] <> drop (today+1) weekInfo)
              (take (today+1) weekInfo <> replicate (6-today) 8)
        rainDays = length $ filter (/=0) weekInfo
        extraInfo = let nextRainCount = length $ takeWhile (== 0) $ drop (today+1) (weekInfo <> weekInfo) in
               (case nextRainCount of
                  0 -> ""
                  1 -> "后天有雨。"
                  _ -> "下次降水将在" <> showT (nextRainCount+1) <> "天后。\n")
            <> "一周内共有" <> showT rainDays <> "天出现降水。\n"

    hours <- getNextRainyHour
    let tmrwRainyHourText = case hours of
                              [] -> ""
                              xs ->
                                    -- let aam = filter (<13) xs
                                        -- ppm = filter (>12) xs
                                        -- genText t hours = (<> t) <$> (hoursToText hours)
                                    -- in
                                    -- ("明日" <>) . (<> "有雨。\n") $
                                    --   mconcat $ intersperse "," $ catMaybes $ zipWith (\f x -> f x)
                                    --                 (genText <$> ["am", "pm"])
                                    --                 [aam, ppm]
                                    fromMaybe "" $
                                      (("明日" <>) . (<> "时有雨。\n")) <$> (hoursToText hours)
    let isLastRain = case rainPcIndices of
                        [0,1] -> True
                        [1] -> True
                        [0] -> True
                        _   -> False
        lastRainInfo = if isLastRain then "明天是未来一周内唯一的雨天。" else ""
    case rainPcIndices of
        [] -> pure Nothing
        _  -> pure . Just $
                  tmrwRainyHourText
               <> lastRainInfo
               <> extraInfo
               <> "------------------\n"
               <> "日 一 二 三 四 五 六\n"
               <> displayInfo (fst breaked) <> "\n"
               <> displayInfo (snd breaked) <> "\n"
               <> "------------------\n"
               <> mconcat (intersperse "\n" $ filter (/= "") (fmap (\(a,b) ->
                  let aExist = elem a rainPcIndices
                      bExist = elem b rainPcIndices in
                 if not (aExist || bExist) then "" else
                    weatherDates !! (div a 2)
                    <> ", "
                    <> (case () of
                      _ | aExist && bExist -> "全天, (" <> (rainPercents !! a) <> ", " <> (rainPercents !! b) <> ")"
                        | aExist -> "日, " <> (rainPercents !! a)
                        | bExist -> "夜, " <> (rainPercents !! b)
                      )
                    ) indicesList))
    where
      -- indicesList = [(0,1),(2,3),(4,5),(6,7),(8,9),
      --   (10,11),(12,13),(14,15),(16,17),(18,19),
      --   (20,21),(22,23),(24,25),(26,27),(28,29)]
      indicesList = take 15 ([(0,1)] <> (zipWith (\(a,b) (c,d) -> (a+c,b+d)) indicesList (repeat (2,2))))
      weekday2Digit i x = case x of
        "周日" -> [i+1,0,0,0,0,0,0]
        "周一" -> [0,i+1,0,0,0,0,0]
        "周二" -> [0,0,i+1,0,0,0,0]
        "周三" -> [0,0,0,i+1,0,0,0]
        "周四" -> [0,0,0,0,i+1,0,0]
        "周五" -> [0,0,0,0,0,i+1,0]
        "周六" -> [0,0,0,0,0,0,i+1]
        _ -> [0,0,0,0,0,0,0]


get7DayScreenshot :: IO (Maybe Text)
get7DayScreenshot = getScreenshot ((770,430), (73,585)) (1280, 1700) "https://weather.com/zh-CN/weather/tenday/l/23.285665,116.148000"