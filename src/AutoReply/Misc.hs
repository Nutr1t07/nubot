{-# LANGUAGE OverloadedStrings #-}
module AutoReply.Misc where

import qualified Data.Text as T
import           Data.Text      (Text)


equalT :: Text -> Text -> Bool
equalT t target = target == trimT t

elemTs :: Text -> [Text] -> Bool
elemTs t targets = trimT t `elem` targets

beginWithT :: Text -> Text -> Bool
beginWithT t target = target == T.take (T.length target) t

trimT' :: Text -> Text -> Text
trimT' ignore = trimEnd ignore . trimHead ignore
    
elemT :: Char -> Text -> Bool
elemT c = T.any (==c)

trimT :: Text -> Text
trimT = trimT' ignore
  where
    ignore :: Text
    ignore = " ,./?!:;~`()-" <>
               "，。、’；：～！？（）" <>
               "的吧了呀也哪呢阿哈呗啊啦哩咧哇耶哉罢呐咯嘛噢呕哟呦"

trimEnd :: Text -> Text -> Text
trimEnd ignore txt =
  if txt /= T.empty && elemT (T.last txt) ignore
    then trimHead ignore $ T.init txt
    else txt

trimHead :: Text -> Text -> Text
trimHead ignore txt =
  if txt /= T.empty && elemT (T.head txt) ignore
    then trimHead ignore $ T.tail txt
    else txt

checkTrueOrFalse :: Text -> Maybe Bool
checkTrueOrFalse txt' = check $ trimT txt'
 where
   check txt
     | txt `elem` trueWords  = Just True
     | txt `elem` falseWords = Just False
     | otherwise = Nothing
   trueWords = [ "是", "嗯", "好", "行", "可以", "没错", "不错", "正确", "确定"]
   falseWords = ["否", "非", "错", "错误"] <> fmap ("不"<>) trueWords