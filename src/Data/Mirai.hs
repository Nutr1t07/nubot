{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Mirai where

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List (intersperse)
import           Database.SQLite.Simple

import           Type.Mirai.Update
import           Type.Mirai.Common
import           Type.Mirai.Request

isFromUser :: Update -> Bool
isFromUser (MUpdate MessageUpdate{..})
  | updm_type `elem` ["TempMessage", "FriendMessage"] = True
  | otherwise = False
isFromUser _ = False

isFromGroup :: Update -> Bool
isFromGroup (MUpdate MessageUpdate{..})
  | updm_type == "GroupMessage" = True
  | otherwise = False
isFromGroup _ = False

isMessage :: Update -> Bool
isMessage (MUpdate _) = True
isMessage _ = False

isEvent :: Update -> Bool
isEvent (EUpdate _) = True
isEvent _ = False

isAddFriendEvent :: Update -> Bool
isAddFriendEvent (EUpdate EventUpdate{upde_type = t}) = t == "NewFriendRequestEvent"
isAddFriendEvent _ = False

isNewMemberEvent :: Update -> Bool
isNewMemberEvent (EUpdate EventUpdate{upde_type = t}) = t == "MemberJoinRequestEvent"
isNewMemberEvent _ = False

isInvitedToGroupEvent :: Update -> Bool
isInvitedToGroupEvent (EUpdate EventUpdate{upde_type = t}) = t == "BotInvitedJoinGroupRequestEvent"
isInvitedToGroupEvent _ = False

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

mkGroupEventResp :: Update -> Int -> Maybe RequestContent
mkGroupEventResp upd@(EUpdate EventUpdate{..}) op =
  if isAddFriendEvent upd
    then Just $ REvent $ Event {ev_eventId = fromJust upde_eventId
                              , ev_fromId  = fromJust upde_fromId
                              , ev_groupId = upde_groupId
                              , ev_operate = op
                              , ev_message = "已处理"}
    else Nothing
mkGroupEventResp _ _ = Nothing

transUpd2SendMsgPQ :: Update -> Text -> Text -> Maybe RequestContent
transUpd2SendMsgPQ upd@(MUpdate MessageUpdate{..}) txt url = (\(RSendMsg x) -> RSendMsg $ x {sm_quote = getMessageId upd}) <$> transUpd2SendMsgP upd txt url
transUpd2SendMsgPQ _ _ _ = Nothing

transUpd2SendMsgP :: Update -> Text -> Text -> Maybe RequestContent
transUpd2SendMsgP upd txt url = (\(RSendMsg x) -> RSendMsg x{sm_messageChain = picChain : sm_messageChain x }) <$> transUpd2SendMsgT upd txt
  where picChain = (emptyChainMessage "Image"){cm_url = Just url}

transUpd2SendMsgT :: Update -> Text -> Maybe RequestContent
transUpd2SendMsgT upd@(MUpdate MessageUpdate{..}) txt =
  let initMsg = case getChatType upd of
                  Just Group  -> Just $ SendMsg Nothing (getGroupId upd) Nothing []
                  Just Temp   -> Just $ SendMsg (getUserId upd) (getGroupId upd) Nothing []
                  Just Friend -> Just $ SendMsg (getUserId upd) Nothing Nothing []
                  Nothing -> Nothing in
  (\x -> RSendMsg $ x {sm_messageChain = (mkMessageChainT txt)}) <$> initMsg
transUpd2SendMsgT upd@(EUpdate EventUpdate{..}) txt 
 | upde_type == "BotInvitedJoinGroupRequestEvent" =
                  Just $ RSendMsg $ SendMsg Nothing (getGroupId upd) Nothing (mkMessageChainT txt)
 | upde_type == "NewFriendRequestEvent" =
    case getUserId upd of
      Just usrId' -> Just $ RSendMsg $ SendMsg (Just usrId') (getGroupId upd) Nothing (mkMessageChainT txt)
      Nothing -> Nothing
 | upde_type == "MemberJoinRequestEvent" =
                  Just $ RSendMsg $ SendMsg Nothing (getGroupId upd) Nothing (mkMessageChainT txt)
 | otherwise = Nothing
transUpd2SendMsgT _ _ = Nothing

mkMessageChainT :: Text -> [ChainMessage]
mkMessageChainT txt = [(emptyChainMessage "Plain") {cm_text = Just txt}]

mkMessageChainTP :: Text -> [Text] -> [ChainMessage]
mkMessageChainTP txt urls = ((emptyChainMessage "Plain") {cm_text = Just txt}) :
  ((\url -> (emptyChainMessage "Image") {cm_url = Just url}) <$> urls)
    

transUpd2SendMsgTQ :: Update -> Text -> Maybe RequestContent
transUpd2SendMsgTQ upd@(MUpdate MessageUpdate{..}) txt = (\(RSendMsg x) -> RSendMsg $ x {sm_quote = getMessageId upd}) <$> transUpd2SendMsgT upd txt
transUpd2SendMsgTQ _ _ = Nothing

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

getReplyId :: Update -> Maybe Integer
getReplyId (MUpdate MessageUpdate{updm_messageChain = cm}) = case filter (\x -> cm_type x == "Quote") cm of
  [] -> Nothing
  (x:_) -> cm_id x
getReplyId _ = Nothing

getUserNick :: Update -> Maybe Text
getUserNick (EUpdate EventUpdate{..}) = upde_nick
getUserNick (MUpdate MessageUpdate{..}) = sdr_nickname updm_sender
getUserNick _ = Nothing

getUserRemark :: Update -> Maybe Text
getUserRemark (MUpdate MessageUpdate{..}) = sdr_remark updm_sender
getUserRemark _ = Nothing

getGroupId :: Update -> Maybe Integer
getGroupId (MUpdate MessageUpdate{..}) = grp_id <$> sdr_group updm_sender
getGroupId (EUpdate EventUpdate{..}) = upde_groupId
getGroupId _ = Nothing

getGroupName :: Update -> Maybe Text
getGroupName (MUpdate MessageUpdate{..}) = grp_name <$> sdr_group updm_sender
getGroupName (EUpdate EventUpdate{..}) = grp_name <$> (sdr_group =<< upde_group)
getGroupName _ = Nothing

getChatType :: Update -> Maybe ChatType
getChatType (MUpdate MessageUpdate{..}) = case updm_type of
  "FriendMessage"   -> Just Friend
  "GroupMessage"    -> Just Group
  "TempMessage"     -> Just Temp
  "StrangerMessage" -> Just Friend
  _                 -> Nothing
getChatType _ = Nothing

-- check from quote
getImgUrls' :: Update -> IO [Text]
getImgUrls' (MUpdate MessageUpdate{..}) = do
  storedMsg <- catMaybes <$> traverse fetchMsg quoteId
  let urls = foldMap message_image_urls storedMsg
  pure $ urls <> directUrls
  where 
    directUrls    = getUrls updm_messageChain
    getUrls chain = map (fromJust . cm_url) $ filter (\x -> cm_type x == "Image") chain
    quoteId       = map (fromJust . cm_id) $ filter (\x -> cm_type x == "Quote") updm_messageChain
getImgUrls' _ = pure []

getImgUrls :: Update -> Maybe [Text]
getImgUrls (MUpdate MessageUpdate{..}) = Just directUrls
  where
    directUrls    = getUrls updm_messageChain
    getUrls chain = map (fromJust . cm_url) $ filter (\x -> cm_type x == "Image") chain
getImgUrls _ = Nothing

getPlainText :: Update -> Maybe Text
getPlainText (MUpdate MessageUpdate{..}) = Just $ foldMap (fromJust . cm_text) $ filter (\x -> cm_type x == "Plain") updm_messageChain
getPlainText _ = Nothing

getText :: Update -> Maybe Text
getText (MUpdate MessageUpdate{..}) = Just $ foldMap toText updm_messageChain
  where
    toText x = case cm_type x of
      "Plain"          -> fromMaybe "[nullText]"  $ cm_text x
      "At"             -> fromMaybe "[nullAt]"    $ cm_display x
      "Quote"          -> foldMap ((<> ")") . ("(Q> "<>) . toText) $ fromMaybe [] (cm_origin x)
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





dbPath = "msgStore.db"

data StoreMsg = StoreMsg {
    user_id :: Integer
  , group_id :: Maybe Integer
  , message_text :: Maybe Text
  , message_image_urls :: [Text]
  , message_type       :: Text
  , message_id         :: Integer
  , reply_id           :: Maybe Integer
  , message_time       :: Integer
} deriving (Eq, Show)
instance ToRow StoreMsg where
  toRow (StoreMsg uid cid mt miu mtp mid rid mtime)
    = toRow (uid, cid, mt, mconcat . intersperse "," $ miu, mtp, mid, rid, mtime)
instance FromRow StoreMsg where
  fromRow =
    StoreMsg
      <$> field
      <*> field
      <*> field
      <*> (T.split (==',') <$> field)
      <*> field
      <*> field
      <*> field
      <*> field
   where
     
    splitOn _ [] = []
    splitOn c str =
      let x = takeWhile (/= c) str in x : splitOn c (drop (length x + 1) str)

mupdToStoreMsg :: Update -> Maybe StoreMsg
mupdToStoreMsg upd = StoreMsg
    <$> getUserId upd
    <*> pure (getGroupId upd)
    <*> pure (getText upd)
    <*> getImgUrls upd
    <*> pure (T.pack (show (getChatType upd)))
    <*> getMessageId upd
    <*> pure (getReplyId upd)
    <*> getMessageTime upd

initMsgLogDB :: IO ()
initMsgLogDB =
  withConnection dbPath (`execute_` createMsgLogDB)

createMsgLogDB :: Query
createMsgLogDB =
  "CREATE TABLE IF NOT EXISTS msgs (user_id INTEGER, \
                                   \group_id INTEGER, \
                                   \message_text TEXT, \
                                   \message_image_urls TEXT, \
                                   \message_type INTEGER, \
                                   \message_id INTEGER, \
                                   \reply_id INTEGER,\
                                   \message_time INTEGER)"


-- | Fetch a logged message from local file "wlMsg.log".
fetchMsg :: Integer -> IO (Maybe StoreMsg)
fetchMsg msgId = do
  r <- withConnection
    dbPath
    (\conn ->
      queryNamed conn
                 "SELECT * FROM msgs WHERE message_id = :msg_id"
                 [":msg_id" := msgId] :: IO [StoreMsg]
    )
  if null r then pure Nothing else pure $ Just (head r)

storeMsg :: Update -> IO ()
storeMsg update = do
  case mupdToStoreMsg update of
    Nothing -> pure ()
    Just msg -> withConnection dbPath
      (\conn -> execute
        conn
        "INSERT OR IGNORE INTO msgs VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        msg
      )