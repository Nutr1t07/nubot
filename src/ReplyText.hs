{-# LANGUAGE OverloadedStrings #-}
module ReplyText where

import Data.Text ( Text )
import System.Random ( randomRIO )

rndPickText :: TextType -> IO Text
rndPickText textType = do
  let texts = rndTextLib textType
  idx <- randomRIO (0, length texts - 1)
  pure (texts !! idx)

data TextType = Hello | OK | WhyCancel | Goodbye

rndTextLib Hello = ["你好", "您好", "嗨"]
rndTextLib OK = ["好的", "了解", "收到"]
rndTextLib WhyCancel = ["真令人沮丧。", "白忙活一场", "真是令人捉摸不透。", "今日的工作内容又是被人调戏"]
rndTextLib Goodbye = ["有缘再会。", "我们的旅程告一段落。", "Farewell.", "相聚总是短暂，分别却是久长。"]