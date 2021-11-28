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
    ( breakOnEnd, breakOn, searchBetweenBL, encodeURI )
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

runBaiduSearch :: Text -> IO (Maybe (Text, Text, Text, Text))
runBaiduSearch query = if query == T.empty
    then return Nothing
    else do
      resp <-  (^. Wreq.responseBody) <$> Wreq.getWith opts "https://www.baidu.com/s"
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
  
  opts =
    Wreq.defaults
      & Wreq.header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:94.0) Gecko/20100101 Firefox/94.0"]
      & Wreq.header "Connection" .~ ["keep-alive" ]
      & Wreq.header "Accept" .~ ["text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"]
      & Wreq.header "Accept-Language" .~ ["zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7"]
      & Wreq.header "Accept-Encoding" .~ ["gzip, deflate, br"]
      & Wreq.header "Cache-Control" .~ ["max-age=0"]
      & Wreq.header "Cookie" .~ ["BAIDUID=858F77E4C160AB7E22BF809094082D94:FG=1; BIDUPSID=858F77E4C160AB7E0B7C30BD1DF06CE1; PSTM=1630820631; __yjs_duid=1_421bf5243711e4318ee861168de3bd4a1630820811466; ISSW=1; ISSW=1; BDUSS=9iLUlMZnY3SnljQWlhUjJRMkIxbzZYOUJRRlBvcHBkc1ZvUmhtZWdWLXNGMzFoRVFBQUFBJCQAAAAAAAAAAAEAAADEydGPTWVtb3J5X0V0ZXJuYWwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAKyKVWGsilVhUm; BD_UPN=133352; NOJS=1; Hm_lvt_aec699bb6442ba076c8981c6dc490771=1636816308; BDSFRCVID=7WIOJexroG0kKanHb0xfboa7K6Kvqf7TDYrEOwXPsp3LGJLVgOBNEG0PtfdgS14-oxmHogKK3gOTH4LF_2uxOjjg8UtVJeC6EG0Ptf8g0M5; H_BDCLCKID_SF=tRk8oK-atDvDqTrP-trf5DCShUFs0x6dB2Q-XPoO3KJOOnjky-K2MfKVhULfB55ibj6h_Mbgy4op8P3y0bb2DUA1y4vpKMRGBeTxoUJ25JbJVIDmqtnWhfkebPRiJ-b9Qg-J2pQ7tt5W8ncFbT7l5hKpbt-q0x-jLTnhVn0MBCK0hD0wDT8hD6PVKgTa54cbb4o2WbCQJtQN8pcN2b5oQT8fyUQXBPDjLeraWbD5WKQvf4jIjpOUWJDkXpJvQnJjt2JxaqRC5h7I8l5jDh3Mb40s0NLJe4ROamby0hvctb3cShPmXMjrDRLbXU6BK5vPbNcZ0l8K3l02V-bIe-t2XjQhDHt8J50ttJ3aQ5rtKRTffjrnhPF3jp-PXP6-hnjy3bRWabj8Wn_2JbQPhPLbyTKU5lQhth3Ry6r42-39LPO2hpRjyxv4bU4YX4oxJpOJ5DnMbKD2HR7WDqnvbURvDP-g3-AJWU5dtjTO2bc_5KnlfMQ_bf--QfbQ0hOhqP-jBRIE3-oJqC-2MK863f; H_PS_PSSID=34447_35104_31254_35054_35048_34584_34505_34532_34916_34578_34606_34815_26350_35074_34868_35114_34994; BD_HOME=1; BDRCVFR[feWj1Vr5u3D]=I67x6TjHwwYf0; delPer=0; BD_CK_SAM=1; PSINO=7; ZD_ENTRY=baidu; Hm_lpvt_aec699bb6442ba076c8981c6dc490771=1636816308; COOKIE_SESSION=491_0_4_1_17_15_1_0_3_3_3_2_37_0_0_0_1636869640_0_1636883230%7C9%230_0_1636883230%7C1; channel=baidusearch; baikeVisitId=890f41d6-a881-40f8-8e1f-d063c9b85e66; BA_HECTOR=8hal010h0l0ga5a1e81gp1mth0q; H_PS_645EC=cf7cvLOcfgwcDZQj%2FzhokEDASz7dOuCAyqXl4hITTSedZyHu31D5pYOUOI1y1qSioWZn; sug=3; sugstore=1; ORIGIN=2; bdime=0; BDSVRTM=0; WWW_ST=1636883639028"]
      & Wreq.param "wd" .~ [query]
      & Wreq.param "ie" .~ ["utf-8"]
      & Wreq.param "pn" .~ ["0"]
      & Wreq.param "cl" .~ ["3"]
      & Wreq.param "rn" .~ ["10"]

runBaikeSearch :: Text -> IO (Maybe Text, Maybe Text)
runBaikeSearch query = do
    url <- getFstUrl <$> Wreq.getWith opts "https://www.baidu.com/s"
    case url of
      Nothing -> pure (Nothing, Nothing)
      Just realUrl -> do
        rst <- getFirstPara <$> Wreq.get realUrl
        logWT Info realUrl
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
  opts =
    Wreq.defaults
      & Wreq.header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:94.0) Gecko/20100101 Firefox/94.0"]
      & Wreq.header "Connection" .~ ["keep-alive" ]
      & Wreq.header "Accept" .~ ["text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"]
      & Wreq.header "Accept-Language" .~ ["zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7"]
      & Wreq.header "Accept-Encoding" .~ ["gzip, deflate, br"]
      & Wreq.header "Cache-Control" .~ ["max-age=0"]
      & Wreq.header "Cookie" .~ ["BAIDUID=858F77E4C160AB7E22BF809094082D94:FG=1; BIDUPSID=858F77E4C160AB7E0B7C30BD1DF06CE1; PSTM=1630820631; __yjs_duid=1_421bf5243711e4318ee861168de3bd4a1630820811466; ISSW=1; ISSW=1; BDUSS=9iLUlMZnY3SnljQWlhUjJRMkIxbzZYOUJRRlBvcHBkc1ZvUmhtZWdWLXNGMzFoRVFBQUFBJCQAAAAAAAAAAAEAAADEydGPTWVtb3J5X0V0ZXJuYWwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAKyKVWGsilVhUm; BD_UPN=133352; NOJS=1; Hm_lvt_aec699bb6442ba076c8981c6dc490771=1636816308; BDSFRCVID=7WIOJexroG0kKanHb0xfboa7K6Kvqf7TDYrEOwXPsp3LGJLVgOBNEG0PtfdgS14-oxmHogKK3gOTH4LF_2uxOjjg8UtVJeC6EG0Ptf8g0M5; H_BDCLCKID_SF=tRk8oK-atDvDqTrP-trf5DCShUFs0x6dB2Q-XPoO3KJOOnjky-K2MfKVhULfB55ibj6h_Mbgy4op8P3y0bb2DUA1y4vpKMRGBeTxoUJ25JbJVIDmqtnWhfkebPRiJ-b9Qg-J2pQ7tt5W8ncFbT7l5hKpbt-q0x-jLTnhVn0MBCK0hD0wDT8hD6PVKgTa54cbb4o2WbCQJtQN8pcN2b5oQT8fyUQXBPDjLeraWbD5WKQvf4jIjpOUWJDkXpJvQnJjt2JxaqRC5h7I8l5jDh3Mb40s0NLJe4ROamby0hvctb3cShPmXMjrDRLbXU6BK5vPbNcZ0l8K3l02V-bIe-t2XjQhDHt8J50ttJ3aQ5rtKRTffjrnhPF3jp-PXP6-hnjy3bRWabj8Wn_2JbQPhPLbyTKU5lQhth3Ry6r42-39LPO2hpRjyxv4bU4YX4oxJpOJ5DnMbKD2HR7WDqnvbURvDP-g3-AJWU5dtjTO2bc_5KnlfMQ_bf--QfbQ0hOhqP-jBRIE3-oJqC-2MK863f; H_PS_PSSID=34447_35104_31254_35054_35048_34584_34505_34532_34916_34578_34606_34815_26350_35074_34868_35114_34994; BD_HOME=1; BDRCVFR[feWj1Vr5u3D]=I67x6TjHwwYf0; delPer=0; BD_CK_SAM=1; PSINO=7; ZD_ENTRY=baidu; Hm_lpvt_aec699bb6442ba076c8981c6dc490771=1636816308; COOKIE_SESSION=491_0_4_1_17_15_1_0_3_3_3_2_37_0_0_0_1636869640_0_1636883230%7C9%230_0_1636883230%7C1; channel=baidusearch; baikeVisitId=890f41d6-a881-40f8-8e1f-d063c9b85e66; BA_HECTOR=8hal010h0l0ga5a1e81gp1mth0q; H_PS_645EC=cf7cvLOcfgwcDZQj%2FzhokEDASz7dOuCAyqXl4hITTSedZyHu31D5pYOUOI1y1qSioWZn; sug=3; sugstore=1; ORIGIN=2; bdime=0; BDSVRTM=0; WWW_ST=1636883639028"]
      & Wreq.param "wd" .~ [query <> " site:baike.baidu.com"]
      & Wreq.param "ie" .~ ["utf-8"]
      & Wreq.param "pn" .~ ["0"]
      & Wreq.param "cl" .~ ["3"]
      & Wreq.param "rn" .~ ["10"]