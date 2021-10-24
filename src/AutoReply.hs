{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module AutoReply where

import Data.Mirai
import Network.Mirai
import Type.Mirai.Update
import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)
import Data.User
import Data.TaskQueue (TaskQueue, addTask)
import ReplyText
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (intersperse)
import Control.Concurrent (threadDelay)
import GHC.Unicode (isDigit)
import Data.Text.Read (decimal)
import Data.Either (fromRight)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Module.WebSearch (runBaiduSearch)
import Module.ImageSearch (runSauceNAOSearch)
import Util.Log (logWT'T, LogTag (Debug, Info), logWT, logErr)
import Type.Mirai.Common (ChatType(Friend))
import Network.Mail (sendUpdateToEmail)
import Data.Mirai (logMsg)

mainHandler :: Int -> TaskQueue -> UserGroup -> RepliedTable -> Update -> Connection -> IO ()
mainHandler selfId tasks grp tb upd conn = do
  logMsg upd
  when (isFromUser upd && fromEnum (fromJust $ getUserId upd) /= selfId) $ addTask tasks $
    privMsgHandler conn grp upd tb
  when (isAddFriendEvent upd) $ addFriendHandler conn grp upd
  when (isFromGroup upd) $ grpMsgHandler conn upd

addFriendHandler :: Connection -> UserGroup -> Update -> IO ()
addFriendHandler conn grp upd@(EUpdate upde) = do
  let userId' = fromJust $ getUserId upd
  usr' <- fetchUser grp userId'
  usr <- case usr' of
            Just x -> pure x
            Nothing -> createUser grp userId'
  let resp = mkFriendEventResp upd 0
  logWT Info ("new friend " <> show userId' <> " added") >> sendCommand "resp_newFriendRequestEvent" conn resp
  reply "嗨，我当前不在线"
  reply "您的好友请求已由机器人自动同意，发送『帮助』以查看此机器人的功能"
  void $ replaceUser grp (setState Idle usr)
  where reply text = sendMessage Friend conn (mkSendMsgT text upd)
addFriendHandler _ _ _ = pure ()

grpMsgHandler :: Connection -> Update -> IO ()
grpMsgHandler conn upd@(MUpdate updm) = do
  case () of
      _ | msgTxtEqTo "sp" -> do
          urls <- getImgUrls' upd
          let imgUrl = case urls of
                [] -> ""
                (x:_)  -> x

          logWT Info $ "running SauceNAO search: " <> show imgUrl
          logWT Debug $ "got image urls: " <> show (getImgUrls upd)
          rst <- runSauceNAOSearch imgUrl
          let txt = case rst of
                      Right x -> x
                      Left err -> err
          replyQ txt
        | msgTxtEqTo "ping" -> replyQ "1 packets transmitted, 1 received, 0% packet loss"
        | otherwise -> pure ()
  where
    msgTxt = fromMaybe "" $ getPlainText upd

    msgTxtEqTo = equalT msgTxt
    -- msgTxtElem = elemT msgTxt

    replyWithText  conn upd text = sendMessage (fromJust $ getChatType upd) conn (mkSendMsgT text upd)
    replyWithTextQ conn upd text = sendMessage (fromJust $ getChatType upd) conn (mkQuoteSendMsgT text upd)
    reply  = replyWithText conn upd
    replyQ = replyWithTextQ conn upd
grpMsgHandler _ _ = pure ()

privMsgHandler :: Connection -> UserGroup -> Update -> RepliedTable -> IO ()
privMsgHandler conn grp upd@(MUpdate updm) tb = do
  let userId' = sdr_id $ updm_sender updm
  usr' <- fetchUser grp userId'
  usr <- case usr' of
            Just x -> pure x
            Nothing -> createUser grp userId'
  changedUsr <- stateHandler usr conn upd tb
  _ <- replaceUser grp (incStage 1 changedUsr) -- stage increase by default
  pure ()
privMsgHandler _ _ _ _ = pure ()

stateHandler :: User -> Connection -> Update -> RepliedTable -> IO User
stateHandler usr conn upd@(MUpdate updm) tb = case state usr of
  Greeting -> do
        hello <- rndPickText Hello
        let rmk = sdr_remark $ updm_sender updm
        -- if sdr_nickname (updm_sender updm) == sdr_remark (updm_sender updm)
          -- then reply "您好，我目前不使用QQ。"
          -- else reply ("嗨，" <> fromMaybe "人类" rmk <> "，很抱歉，我目前不使用QQ。")
        -- reply "您可以通过此QQ向我留言，如果有特殊或紧急事件，请直接获取我的联系方式。" >> threadDelay 3000000
        -- reply "通常情况下，我建议您『留言』。您的留言将会被合并成一封电子邮件自动发送至我的邮箱，我将在看到后尽快回复您。" >> threadDelay 6000000
        -- reply "如果您想要留言，请发送『留言』；如果您想要获取我的联系方式，请发送『联系方式』。或发送『帮助』以获取提示。"
        reply "嗨，我现在似乎并不在线呢。"
        reply "不过你可以跟这个机器人先玩一会儿！"
        reply "或者...发送『留言』来给我留言。"
        pure $ setState Idle usr

  Recording -> case stage usr of
    -- recording msg
    0 -> case () of
      _ | msgTxtElem ["结束", "完毕", "完成"] -> do
            if null $ recordedMsg usr
              then replyQ "已退出留信模式，没有记录到任何消息。" >> pure (setState Idle usr)
              else do
                replyQ "已退出留信模式。记录的消息如下："
                reply $ mconcat $ intersperse "\n" $
                          zipWith ((\a b -> "(" <> a <> ") " <> b) . T.pack . show) [1..] $
                            fmap (fromJust . getText) (reverse $ recordedMsg usr)
                replyOnce "请注意，若您的留言中含有图片、文件，它们将会以附件的形式发送(尽管在此处不会显示)。"
                replyOnce "『追加』继续添加内容\n『编辑』指定消息编号修改内容\n\n『发送』发送消息\n『取消』取消并清空所有消息"
                pure (jmpStage 1 usr{recordedMsg = recordedMsg usr})
        | msgTxtEqTo "撤回" -> do
            if null $ recordedMsg usr
              then replyQ "还没有记录到信息。" >> pure (jmpStage 0 usr)
              else replyQ ("最后记录的一条内容已被删除。该消息内容为：\n『" <> (fromJust . getText) (head (recordedMsg usr)) <>  "』")
                   >> pure (jmpStage 0 usr{recordedMsg = tail $ recordedMsg usr})
        | otherwise -> pure (jmpStage 0 usr{recordedMsg = upd : recordedMsg usr})

    -- finished record
    1 -> case () of
      _ | msgTxtEqTo "取消" -> do
            replyQ "操作已取消，所有留言已清空。"
            ct <- utcTimeToPOSIXSeconds <$> getCurrentTime
            let lt = fromInteger $ fromMaybe 0 $ getMessageTime $ last (recordedMsg usr)
            if ct - lt  > 3 * 60
              then do
                t <- rndPickText WhyCancel
                reply t
              else pure ()
            pure (setState Idle usr{recordedMsg = []})
        | msgTxtEqTo "发送" -> do
            sendUpdateToEmail (recordedMsg usr)
            replyQ "消息已发送。"
            threadDelay 10000000
            goodbye <- rndPickText Goodbye
            replyOnce goodbye
            pure (setState Idle usr{recordedMsg = []})
        | msgTxtElem ["追加", "添加"] -> do
            replyQ "请继续发送需要记录的消息。输入『结束』以退出。"
            pure (jmpStage 0 usr)
        | snd (T.breakOn "编辑" (trimT msgTxt)) /= T.empty -> do 
            let trimmedTxt = trimT msgTxt
            if trimmedTxt == "编辑"
              then replyQ "请指定上述记录的消息编号。输入『取消』以退出。" >> pure usr
              else do
                let id' = fst $ fromRight (-1, T.empty) $ decimal $ 
                            T.takeWhile isDigit $ T.dropWhile (not . isDigit) trimmedTxt
                if id' < 1
                  then do
                    replyQ "请指定上述记录的消息编号。输入『取消』以退出。"
                    pure usr
                  else do
                    replyQ ("您正在编辑消息(" <> T.pack (show id') <> ")，发送文本以替换该编号对应的消息。输入『取消』以退出")
                    pure (addFlag (Editing id') $ jmpStage 3 usr)
        | otherwise -> do
            replyQ "未能识别您的回复，请输入『追加』、『编辑』、『发送』或『取消』"
            pure (jmpStage 1 usr)

    2 -> case () of
        _ | msgTxtEqTo "取消" -> do
            replyQ "已退出编辑模式。一条待发送的消息需要处理，请输入『追加』、『编辑』、『发送』或『取消』"
            pure (jmpStage 1 usr)
          | otherwise -> do
              let trimmedTxt = trimT msgTxt
                  id' = fst $ fromRight (-1, T.empty) $ decimal $ 
                          T.takeWhile isDigit $ T.dropWhile (not . isDigit) trimmedTxt
              if id' < 1
                then do
                  replyQ "无法识别的编号，请重新输入。"
                  pure (jmpStage 2 usr)
                else do
                  replyQ ("您正在编辑消息(" <> T.pack (show id') <> ")，发送文本以替换该编号对应的消息。输入『取消』以退出")
                  pure (addFlag (Editing id') $ jmpStage 3 usr)

    3 -> case () of
        _ | msgTxtEqTo "取消" -> do
              replyQ "已退出编辑模式。一条待发送的消息需要处理，请输入『追加』、『编辑』、『发送』或『取消』"
              pure (jmpStage 1 usr)
          | otherwise -> do
              let findEditId ((Editing id'):_) = id'
                  findEditId [] = -1
                  findEditId (x:xs) = findEditId xs
                  id' = findEditId (flag usr)
              reply ("消息("<> T.pack (show id') <>")已被替换为如下内容：\n" <> msgTxt)
              let replaceMsg _ [] _ _       = []
                  replaceMsg r (x:xs) i idx = if i == idx
                                                then r : xs
                                                else x : replaceMsg r xs (i+1) idx
                  replaced = replaceMsg upd (recordedMsg usr) 1 id'
              reply $ ("效果如下：\n" <>) $ mconcat $ intersperse "\n" $
                        zipWith ((\a b -> "(" <> a <> ") " <> b) . T.pack . show) [1..] $
                          fmap (fromJust . getText) (reverse replaced)
              pure (jmpStage 1 usr{recordedMsg = replaced})

    _ -> pure usr

  Searching -> case () of
    _ | msgTxtElem ["退出", "结束"] -> do
          replyQ "已退出搜索模式。"
          pure (setState Idle usr)
      | otherwise ->  do
          rst <- runBaiduSearch msgTxt
          case rst of
            (Just txt, Just url) -> reply $ "- 百度百科\n\n" <> txt <> "\n\n" <> url
            (_       , Nothing)  -> reply "- 百度百科\n\n无结果"
            (Nothing , Just url) -> reply $ "- 百度百科\n\n查询到结果，但没有摘要，请访问该页面以查看结果\n\n" <> url
          pure usr

  Idle -> case () of
         _ | msgTxtElem ["留信", "留言"] -> do
                 replyQ "已进入留信模式，您接下来发送的每一条消息都将会被记录。"
                 replyOnce "『撤回』删除您上一条发送的消息\n『结束』退出留信模式，之后您可以对已存储的留信进行查看与修改。"
                 pure (setState Recording usr)
           | msgTxtEqTo "搜索" -> do
                 replyQ "已进入搜索模式，请发送欲查询的内容。输入『退出』以退出。"
                 pure (setState Searching usr)
           | msgTxtElem ["帮助"] -> do
                reply "『留言』\n\
                      \    进入留言模式。\n\n\
                      \『联系方式』\n\
                      \    获取我的联系方式。请注意，当您获取我的联系方式时，我将收到一条通知。\n\n\
                      \『搜索』(待完善)\n\
                      \    这是一项试验性功能，您可以通过我获取简单的摘要信息。\n\n\
                      \『关于』\n\
                      \    获取此项目的一些说明。\
                      \"
                pure usr
           | msgTxtEqTo "关于" -> do
                -- replyOnce "「关于」\n\n\
                --           \自动应答机器人，项目代号NuBot，使用函数式编程语言Haskell编写，托管于CloudCone洛杉矶服务器。\n\n\
                --           \我想要让自己脱离社交软件与即时通讯软件的束缚，它们已经消耗我太多时间了。如果能得到正确的使用，这种留言的方式将能够些许提升交流的效率。\n\n\
                --           \如果这样为您带来任何的不适，我感到十分抱歉。"
                pure usr
           | msgTxtEqTo "ping" -> do
                replyQ "pong!"
                pure usr
           | msgTxtEqTo "联系方式" -> do
                replyOnce "这个功能暂时关闭，但是您的请求已经上报；我将多加关注您的信息。"
                replyOnce "您也可以直接发送邮件至我的邮箱"
                reply "ne1s07@outlook.com"
                pure usr
           | otherwise -> replyOnce "无法处理的非指令消息。发送『帮助』以查看此机器人的功能。" >> pure usr

  where
    msgTxt = fromMaybe "" $ getPlainText upd

    msgTxtEqTo = equalT msgTxt
    msgTxtElem = elemT msgTxt

    replyWithText  conn upd text = sendMessage (fromJust $ getChatType upd) conn (mkSendMsgT text upd)
    replyWithTextQ conn upd text = sendMessage (fromJust $ getChatType upd) conn (mkQuoteSendMsgT text upd)
    reply  = replyWithText conn upd
    replyQ = replyWithTextQ conn upd
    replyOnce  = replyOnce' reply
    replyOnceQ = replyOnce' replyQ

    replyOnce' f txt = do
      replied <- isReplied tb txt (userId usr)
      if replied
         then pure ()
         else f txt >> markReplied tb txt (userId usr)
stateHandler usr _ _ _ = pure usr



data NameError = TooLong | TooShort | ContainDigit

-- util func
setState :: State -> User -> User
setState s usr = jmpStage 0 usr{state = s}

jmpStage :: Integer -> User -> User
jmpStage n usr = usr{stage = n - 1}

incStage :: Integer -> User -> User
incStage n usr = usr{stage = stage usr + n}

rmvFlag :: Flag -> User -> User
rmvFlag  f usr = usr{flag = filter (/= f) (flag usr)}

addFlag :: Flag -> User -> User
addFlag  f usr = usr{flag = let fs = flag usr in if f `elem` fs then fs else f:fs}


-- trival func

checkRemark txt
  | T.length txt > 4 = Left TooLong
  | T.length txt < 2 = Left TooShort
  | T.any isDigit txt = Left ContainDigit
  | otherwise = Right txt

nameErrorReply TooLong = "看起来有点长...您确定要这样叫吗？"
nameErrorReply TooShort = "似乎有些太短了，这就是您的名字吗？"
nameErrorReply ContainDigit = "这个名字里含有数字，确定这是您的称呼吗？"

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