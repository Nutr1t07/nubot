{-# LANGUAGE OverloadedStrings #-}
module Module.ImageSearch where

import           Control.Exception              ( SomeException (SomeException)
                                                , try
                                                )
import           Control.Lens                   ( (&)
                                                , (.~)
                                                , (^.)
                                                , (?~)
                                                )
import           Control.Monad                  ( join )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(parseJSON)
                                                , eitherDecode
                                                , withObject
                                                )
import           Data.Either                    ( fromRight )
import           Data.Maybe                     ( catMaybes, isNothing, fromJust
                                                )
import           Data.Text                      ( Text
                                                )
import qualified Data.Text                     as T
import           Data.Text.Read                 ( decimal )
import           Network.Wreq                  as Wreq
                                                ( defaults
                                                , getWith
                                                , param
                                                , responseBody
                                                , proxy
                                                , httpProxy, get
                                                )
import           Util.Log                       ( LogTag(Info, Debug)
                                                , logWT, logWT'C8
                                                )
import qualified Data.ByteString.Lazy as BL
import           Util.Misc                      as Misc
                                                ( unlines, searchBetweenBL )
import Type.Mirai.Update (Update)
import Data.Monads (EitherT(runEitherT, EitherT), modErr, MonadTrans (lift), exitErr)
import Data.Mirai (getImgUrls)
import qualified Data.Text.Encoding as T (decodeUtf8)

runSauceNAOSearch :: Text -> IO (Either Text (Text,Text,Maybe Text))
runSauceNAOSearch url = runEitherT $ do
  _ <- if url == T.empty then exitErr "无法获取图片地址。" else pure ()
  r <- modErr getErrHint $
         try $ Wreq.getWith opts "https://saucenao.com/search.php"

  rst' <- modErr (T.pack . ("解析JSON错误: " <>))
             (pure $ eitherDecode (r ^. responseBody))

  rst <- case sr_results rst' of
           [] -> exitErr "SauceNAO 没有返回任何结果。"
           x -> pure $ head x

  let highestSimilarity =
        fromRight 0 $ fst <$> decimal (sr_similarity rst) :: Int
  c <- if highestSimilarity > 70
    then pure Nothing
    else do
      urls' <- lift $ getSearchUrls url
      case urls' of
        Left err -> pure $ Just err
        Right x ->  pure $ Just x

  let (a,b) = getInfo rst
  pure (a,b,c)

 where
  getErrHint :: SomeException -> Text
  getErrHint excp =
    if snd (T.breakOn "rate limit" (T.pack $ show excp)) == ""
      then "请求错误: " <> T.pack (show excp)
      else "技能冷却中！请稍后重试"
  apiKey = "d4c5f40172cb923c73c409538f979482a469d5a7"
  opts =
    defaults
      -- &  proxy ?~ httpProxy "localhost" 10809
      &  param "db"
      .~ ["999"]
      &  param "output_type"
      .~ ["2"]
      &  param "numres"
      .~ ["1"]
      &  param "api_key"
      .~ [apiKey]
      &  param "url"
      .~ [url]

getInfo :: SnaoResult -> (Text, Text)
getInfo sRst =
  let similarity = pure $ sr_similarity sRst
      source     = head <$> sr_ext_url sRst
      siteDomain = T.takeWhile (/= '/') . T.drop 2 . T.dropWhile (/= '/') <$> source
      title     = sr_title sRst
      pixiv_mem = sr_pixiv_member sRst
      doujinshi = sr_doujinshi_name sRst
      thumbnail = sr_thumbnail sRst in

  (,) thumbnail $
  Misc.unlines $ catMaybes $  [Just "# SauceNAO"]
    <> mkInfo "相似度" similarity
    <> mkInfo "图源"  source
    <> mkInfo "标题"  title
    <> mkInfo "画师"  pixiv_mem
    <> mkInfo "本子"  doujinshi
  where mkInfo key value = (: []) $ ((key <> "> ") <>) <$> value


data SnaoResults = SnaoResults
  { sh_short_remaining :: Int
  , sh_long_remaining  :: Int
  , sh_status          :: Int
  , sr_results         :: [SnaoResult]
  }
  deriving Show
instance FromJSON SnaoResults where
  parseJSON = withObject "SnaoResults" $ \v ->
    SnaoResults
      <$> ((v .: "header") >>= (.: "short_remaining"))
      <*> ((v .: "header") >>= (.: "long_remaining"))
      <*> ((v .: "header") >>= (.: "status"))
      <*> (v .: "results")

data SnaoResult = SnaoResult
  { sr_similarity     :: T.Text
  , sr_thumbnail      :: T.Text
  , sr_ext_url        :: Maybe [T.Text]
  , sr_title          :: Maybe T.Text
  , sr_doujinshi_name :: Maybe T.Text
  , sr_pixiv_member   :: Maybe T.Text
  , sr_pixiv_id       :: Maybe Integer
  }
  deriving Show
instance FromJSON SnaoResult where
  parseJSON = withObject "SnaoResult" $ \v ->
    SnaoResult
      <$> ((v .: "header") >>= (.: "similarity"))
      <*> ((v .: "header") >>= (.: "thumbnail"))
      <*> ((v .: "data") >>= (.:? "ext_urls"))
      <*> ((v .: "data") >>= (.:? "title"))
      <*> ((v .: "data") >>= (.:? "jp_name"))
      <*> ((v .: "data") >>= (.:? "member_name"))
      <*> ((v .: "data") >>= (.:? "pixiv_id"))


getAscii2dUrls :: Text -> IO (Maybe (Text, Text))
getAscii2dUrls imgUrl = do
  r <- Wreq.get $ "https://ascii2d.net/search/url/" <> T.unpack imgUrl
  let rContent = r ^. responseBody
  let colorUrl =
        fixUrl . ("color" <>) <$> Misc.searchBetweenBL "color" "\"" rContent
  let bovwUrl =
        fixUrl . ("bovw" <>) <$> Misc.searchBetweenBL "bovw" "\"" rContent
  pure $ (,) <$> colorUrl <*> bovwUrl
  where fixUrl =  T.decodeUtf8 . BL.toStrict . ("https://ascii2d.net/search/" <>)

getSearchUrls :: Text -> IO (Either Text Text)
getSearchUrls imgUrl = runEitherT $ do
    if imgUrl == T.empty then exitErr "无法获取图片地址。" else pure ()
    result  <- lift $ getAscii2dUrls imgUrl
    if isNothing result then exitErr "Ascii2d 没有返回结果" else pure ()
    pure $ Misc.unlines
      [ "A2d色合> " <> fst (fromJust result)
      , "A2d特征> " <> snd (fromJust result)
      , "Yandex> " <> yandexHost <> imgUrl
      ]
 where
  
  yandexHost =
    "https://yandex.com/images/search?source=collections&rpt=imageview&url="