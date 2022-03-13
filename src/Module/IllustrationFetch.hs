{-# LANGUAGE OverloadedStrings #-}
module Module.IllustrationFetch where

import           Control.Lens                   ( (&)
                                                , (.~)
                                                , (^.)
                                                )
import qualified Data.ByteString.Lazy          as BL
import           Data.ByteString.Lazy.Search   as BL
                                                ( breakAfter
                                                , breakOn
                                                )
import qualified Data.ByteString.Lazy.UTF8     as UTF8
                                                ( toString )
import           Data.List                      ( intersperse )
import           Data.Text                      ( Text
                                                , strip
                                                , unpack
                                                )
import qualified Data.Text                     as T
import           Data.Text.Lazy                 ( toStrict )
import           Data.Text.Lazy.Encoding        ( decodeUtf8 )
import qualified Network.Wreq                  as Wreq
import           Data.Maybe                     (fromMaybe)
import           Util.Misc                     as Misc
                                                ( breakOnEnd
                                                , breakOn
                                                , searchAllBetweenBL
                                                , searchBetweenBL
                                                , searchBetweenBL'
                                                , encodeURI )
import           Util.Log                       ( logWT, LogTag(Info) )

import           Type.Mirai.Common              ( ChainMessage(ChainMessage) )
import           Data.Mirai                     ( mkMessageChainTP )

splitIntoGroups :: Int -> [a] -> [[a]]
splitIntoGroups _ [] = []
splitIntoGroups i xs = if length xs > i then (take (i+1) xs) : splitIntoGroups i (drop (i+1) xs) else [xs]

fetchYandeRe24h_out :: IO [[ChainMessage]]
fetchYandeRe24h_out = do
  urls <- fetchYandeRe24h
  pure [(mkMessageChainTP T.empty url ) | url <- splitIntoGroups 6 urls]


fetchYandeReWeek_out :: IO [[ChainMessage]]
fetchYandeReWeek_out = do
  urls <- fetchYandeReWeek
  pure [(mkMessageChainTP T.empty url) | url <- splitIntoGroups 6 urls]

fetchYandeReWeek :: IO [Text]
fetchYandeReWeek = do
      resp <- (BL.drop 10000) . (^. Wreq.responseBody) <$> Wreq.get "https://yande.re/post/popular_by_week"
      let urlList = take 40 $ searchAllBetweenBL "href=\"https://files.yande.re/" "\"" resp
      pure $ ( "https://files.yande.re/sample/" <> )
           . ( <> ".re.jpg")
           . ( T.takeWhile (/='.') )
           . ( T.drop 1 . T.dropWhile (/='/') )
           . toStrict . decodeUtf8 <$> urlList

fetchYandeRe24h :: IO [Text]
fetchYandeRe24h = do
      resp <- (BL.drop 10000) . (^. Wreq.responseBody) <$> Wreq.get "https://yande.re/post/popular_recent"
      let urlList = take 40 $ searchAllBetweenBL "href=\"https://files.yande.re/" "\"" resp
      pure $ ( "https://files.yande.re/sample/" <> )
           . ( <> ".re.jpg")
           . ( T.takeWhile (/='.') )
           . ( T.drop 1 . T.dropWhile (/='/') )
           . toStrict . decodeUtf8 <$> urlList