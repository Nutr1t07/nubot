{-# LANGUAGE OverloadedStrings #-}
module Module.WebSearch where

import           Util.Log                       ( LogTag(Info)
                                                , logWT
                                                )
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
import qualified Data.Text                     as Text
import           Data.Text.Lazy                 ( toStrict )
import           Data.Text.Lazy.Encoding        ( decodeUtf8 )
import           Network.Wreq                   ( defaults
                                                , get
                                                , getWith
                                                , header
                                                , param
                                                , responseBody
                                                )
import Data.Maybe (fromMaybe)
import Util.Misc as Misc ( searchBetweenBL, breakOn, breakOnEnd )
import Network.URI.Encode as URI ( encode )

getWords :: BL.ByteString -> [Text]
getWords ""  = []
getWords str = (strip . toStrict . decodeUtf8 $ fst (BL.breakOn "<" xs))
  : getWords xs
  where xs = snd $ breakAfter ">" str

-- Select fragments that are not equal to "&nbsp;" or started with "\n"
concatWord :: [Text] -> Text
concatWord oStr = (mconcat . intersperse "\n\n") s
 where
  s = foldr addNextLine []
    $ filter (\str -> str /= "&nbsp;" && str /= "" && Text.head str /= '[') oStr
  addNextLine x [] = [x]
  addNextLine x xs =
    let a = strip x
    in  if Text.last a == '。' then a : xs else (a <> head xs) : tail xs

runBaiduSearch :: Text -> IO (Maybe Text, Maybe Text)
runBaiduSearch query = do
    url <- getFstUrl <$> getWith opts "https://www.baidu.com/s"
    case url of
      Nothing -> pure (Nothing, Nothing)
      Just realUrl -> do
        rst <- getFirstPara <$> get realUrl
        let url' = Text.pack $ replaceBetween "https://baike.baidu.com/item/" "/" realUrl URI.encode
        case rst of
          Nothing -> pure (Nothing, Just url')
          Just realRst -> pure (Just (concatWord . getWords $ realRst), Just url')

 where  
  replaceBetween left right txt f =
    let (oriLeft, x) = Misc.breakOnEnd left txt
        (middle, remain) = Misc.breakOn right x in
    oriLeft <> f middle <> remain

  getFirstPara x = searchBetweenBL
    "<div class=\"lemma-summary\" label-module=\"lemmaSummary\""
    "</div>\n</div>"
    (x ^. responseBody)
  getFstUrl content =
    ("https://baike.baidu.com/item" <>) . UTF8.toString <$> searchBetweenBL
      "baike.baidu.com/item"
      "\""
      (BL.drop 180000 (content ^. responseBody))
  opts =
    defaults
      &  header "User-Agent"
      .~ [ "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:34.0) Gecko/20100101 Firefox/73.0"
         ]
      &  param "wd"
      .~ [query <> " site:baike.baidu.com"]
      &  param "ie"
      .~ ["utf-8"]
      &  param "pn"
      .~ ["0"]
      &  param "cl"
      .~ ["3"]
      &  param "rn"
      .~ ["100"]