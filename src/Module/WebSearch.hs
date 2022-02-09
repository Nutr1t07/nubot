{-# LANGUAGE OverloadedStrings #-}
module Module.WebSearch where

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
import Data.Maybe (fromMaybe)
import Util.Misc as Misc
    ( breakOnEnd, breakOn, searchBetweenBL, searchBetweenBL', encodeURI )
import Util.Log ( logWT, LogTag(Info) )

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
    $ filter (\str -> str /= "&nbsp;" && str /= "" && T.head str /= '[') oStr
  addNextLine x [] = [x]
  addNextLine x xs =
    let a = strip x
    in  if T.last a == '。' then a : xs else (a <> head xs) : tail xs
  
runGoogleSearch :: Text -> IO (Maybe (Text, Text, Text))
runGoogleSearch query = if query == T.empty
  then return Nothing
  else do
      resp <- (^. Wreq.responseBody) <$> Wreq.getWith (googleOpts query) "https://www.google.com.hk/search"
      Prelude.writeFile "test.html" $ UTF8.toString resp
      let ans = getFstAns resp
      pure $ Just
        ( getLink ans
        , getTitle ans
        , getAbstract ans)
 where

  sbl a b c = fromMaybe "" $ searchBetweenBL a b c 
  sbl' a b c = fromMaybe "" $ searchBetweenBL' a b c 

  getAbstract x = concatWord . getWords $ sbl
    "clamp:2\">"
    "</span"
    x

  getLink x = toStrict . decodeUtf8 $ sbl
    "href=\""
    "\""
    x

  getTitle x = toStrict . decodeUtf8 $ sbl'
    ">"
    "</h3><div "
    x

  getFstAns x = sbl
    "style=\"flex-shrink:0\">"
    "</span></div></div><div"
    x

googleOpts query =  Wreq.defaults
                  & Wreq.header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:94.0) Gecko/20100101 Firefox/94.0"]
                  & Wreq.header "Accept" .~ ["text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"]
                  & Wreq.header "Accept-Language" .~ ["zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7"]
                  & Wreq.param "q" .~ [query]
                  & Wreq.param "client" .~ ["firefox-b-d"]

runBaiduSearch :: Text -> IO (Maybe (Text, Text, Text, Text))
runBaiduSearch query = if query == T.empty
    then return Nothing
    else do
      resp <- (^. Wreq.responseBody) <$> Wreq.getWith (baiduOpts query) "https://www.baidu.com/s"
      Prelude.writeFile "test.html" $ UTF8.toString resp
      let ans = getFstAns resp
      pure $ Just
        ( getLink ans
        , getTitle ans
        , getAbstract ans
        , T.pack ("https://www.baidu.com/s?wd=" <> Misc.encodeURI (T.unpack query)))
 where
  replaceBetween left right txt f =
    let (oriLeft, x) = Misc.breakOnEnd left txt
        (middle, remain) = Misc.breakOn right x in
    oriLeft <> f middle <> remain

  sbl a b c = fromMaybe "" $ searchBetweenBL a b c 

  getAbstract x = concatWord . getWords $ sbl
    "abstract"
    "<style>"
    x

  getLink x = toStrict . decodeUtf8 $ sbl
    "href = \""
    "\""
    x

  getTitle x = concatWord . getWords $ sbl
    "<em"
    "</a>"
    x

  getFstAns x = sbl
    "result c-container new-pmd"
    "</div><style>"
    x
  
baiduOpts query =  Wreq.defaults
      & Wreq.header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:94.0) Gecko/20100101 Firefox/94.0"]
      & Wreq.header "Accept" .~ ["text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"]
      & Wreq.header "Accept-Language" .~ ["zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7"]
      & Wreq.header "Cookie" .~ ["BAIDUID=286E1C8A577310DD6306E2FF43F544AB:FG=1; BIDUPSID=286E1C8A577310DD6306E2FF43F544AB; PSTM=1643461863; NOJS=1; delPer=0; BD_CK_SAM=1; PSINO=1; BDSVRTM=12; BD_HOME=1; H_PS_PSSID=35104_31254_35775_34584_35490_35796_35316_26350_35765"]
      & Wreq.param "wd" .~ [query]
      & Wreq.param "ie" .~ ["utf-8"]
      & Wreq.param "cl" .~ ["3"]
      & Wreq.param "rn" .~ ["10"]

runBaikeSearch :: Text -> IO (Maybe Text, Maybe Text)
runBaikeSearch query = if query == T.empty
  then return (Nothing, Nothing)
  else do
    let query' = query <> " site:baike.baidu.com"
    url <- getFstUrl <$> Wreq.getWith (baiduOpts query') "https://www.baidu.com/s"
    case url of
      Nothing -> pure (Nothing, Nothing)
      Just realUrl -> do
        rst <- getFirstPara <$> Wreq.get realUrl
        let url' = T.pack $ Misc.encodeURI realUrl
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
    (x ^. Wreq.responseBody)
  getFstUrl content =
    ("https://baike.baidu.com/item" <>) . UTF8.toString <$> searchBetweenBL
      "baike.baidu.com/item"
      "\""
      (BL.drop 180000 (content ^. Wreq.responseBody))
