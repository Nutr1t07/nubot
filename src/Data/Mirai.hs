{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Mirai where

import Type.Mirai.Update
import Type.Mirai.Common
import Data.Maybe
import Data.Text ( Text )
import Type.Mirai.Request

isFromUser :: Update -> Bool 
isFromUser (MUpdate MessageUpdate{..}) 
  | updm_type `elem` ["TempMessage", "FriendMessage"] = True
  | otherwise = False
isFromUser _ = False

isEvent :: Update -> Bool
isEvent (EUpdate _) = True
isEvent _ = False

isAddFriendEvent :: Update -> Bool
isAddFriendEvent (EUpdate EventUpdate{upde_type = t}) = t == "NewFriendRequestEvent"
isAddFriendEvent _ = False

mkFriendEventResp :: Update -> Int -> Maybe RequestContent
mkFriendEventResp upd@(EUpdate EventUpdate{..}) op =
  if isAddFriendEvent upd
    then Just $ REvent $ Event {ev_eventId = fromJust upde_eventId
                              , ev_fromId = fromJust upde_fromId
                              , ev_groupId = upde_groupId
                              , ev_operate = op
                              , ev_message = "已处理"}
    else Nothing
mkFriendEventResp _ _ = Nothing

mkTextMessageChain :: Text -> ChainMessage
mkTextMessageChain txt = (emptyChainMessage "Plain") {cm_text = Just txt}

mkSendMsgT :: Text -> Update -> Maybe RequestContent
mkSendMsgT txt upd@(MUpdate MessageUpdate{..}) =
  let initMsg = case getChatType upd of
                  Just Group  -> Just $ SendMsg Nothing (getGroupId upd) Nothing []
                  Just Temp   -> Just $ SendMsg (getUserId upd) (getGroupId upd) Nothing []
                  Just Friend -> Just $ SendMsg (getUserId upd) Nothing Nothing []
                  Nothing -> Nothing in
  (\x -> RSendMsg $ x {sm_messageChain = [mkTextMessageChain txt]}) <$> initMsg
mkSendMsgT txt upd@(EUpdate EventUpdate{..}) = case getUserId upd of
  Just usrId' -> Just $ RSendMsg $ SendMsg (Just usrId') (getGroupId upd) Nothing [mkTextMessageChain txt]
  Nothing -> Nothing
mkSendMsgT _ _ = Nothing

mkQuoteSendMsgT :: Text -> Update -> Maybe RequestContent
mkQuoteSendMsgT txt upd@(MUpdate MessageUpdate{..}) = (\(RSendMsg x) -> RSendMsg $ x {sm_quote = getMessageId upd}) <$> mkSendMsgT txt upd
mkQuoteSendMsgT _ _ = Nothing

getMessageTime :: Update -> Maybe Integer 
getMessageTime (MUpdate MessageUpdate{updm_messageChain = chain})= cm_time $ head chain
getMessageTime _ = Nothing

getMessageId :: Update -> Maybe Integer
getMessageId (MUpdate MessageUpdate{..}) = cm_id $ head updm_messageChain
getMessageId (EUpdate EventUpdate{..}) = upde_groupId
getMessageId _ = Nothing

getUserId :: Update -> Maybe Integer
getUserId (MUpdate MessageUpdate{..}) = Just $ sdr_id updm_sender
getUserId (EUpdate EventUpdate{..}) = upde_fromId
getUserId _ = Nothing

getGroupId :: Update -> Maybe Integer
getGroupId (MUpdate MessageUpdate{..}) = grp_id <$> sdr_group updm_sender
getGroupId (EUpdate EventUpdate{..}) = upde_groupId
getGroupId _ = Nothing

getChatType :: Update -> Maybe ChatType
getChatType (MUpdate MessageUpdate{..}) = case updm_type of
  "FriendMessage"   -> Just Friend
  "GroupMessage"    -> Just Group
  "TempMessage"     -> Just Temp
  "StrangerMessage" -> Just Friend
  _                 -> Nothing
getChatType _ = Nothing 

getPlainText :: Update -> Maybe Text
getPlainText (MUpdate MessageUpdate{..}) = Just $ foldMap (fromJust . cm_text) $ filter (\x -> cm_type x == "Plain") updm_messageChain
getPlainText _ = Nothing

getText :: Update -> Maybe Text
getText (MUpdate MessageUpdate{..}) = Just $ foldMap toText updm_messageChain
  where
    toText x = case cm_type x of
      "Plain"          -> fromMaybe "[nullText]"  $ cm_text x
      "At"             -> fromMaybe "[nullAt]"    $ cm_display x
      "Quote"          -> maybe "[nullQuote]" ((<> ")") . ("(Q> "<>) . toText) (cm_origin x)
      "AtAll"          -> "[@全体成员]"
      "Face"           -> "[表情]"
      "Image"          -> "[图片]"
      "FlashImage"     -> "[闪照]"
      "Voice"          -> "[语音]"
      "Xml"            -> "[XML]"
      "Json"           -> "[JSON]"
      "App"            -> "[App]"
      "Poke"           -> "[戳一戳]"
      "MusicShare"     -> "[音乐]"
      "ForwardMessage" -> "[转发的信息]"
      "File"           -> "[文件]"
      "Source"         -> ""
      _                -> "[不支持的消息]"
getText _ = Nothing