{-# LANGUAGE OverloadedStrings #-}
module AutoReply.Misc where

import qualified Data.Text as T
import           Data.Text      (Text)


equalT t target = target == trimT t
elemT t targets =  trimT t `elem` targets

trimT :: Text -> Text
trimT txt = trimEnd $ trimHead txt
  where
    elemT c t = T.any (==c) t
    trimEnd txt =
      if txt /= T.empty && elemT (T.last txt) ignore
        then trimHead $ T.init txt
        else txt
    trimHead txt =
      if txt /= T.empty && elemT (T.head txt) ignore
        then trimHead $ T.tail txt
        else txt
    ignore :: Text
    ignore = " ,./?!:;'~`()-" <>
               "，。、’；：～！？（）" <>
               "的吧了呀也哪呢阿哈呗啊啦哩咧哇耶哉罢呐咯嘛噢呕哟呦"


checkTrueOrFalse :: Text -> Maybe Bool
checkTrueOrFalse txt' = check $ trimT txt'
 where
   check txt
     | txt `elem` trueWords  = Just True
     | txt `elem` falseWords = Just False
     | otherwise = Nothing
   trueWords = [ "是", "嗯", "好", "行", "可以", "没错", "不错", "正确", "确定"]
   falseWords = ["否", "非", "错", "错误"] <> fmap ("不"<>) trueWords