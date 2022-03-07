{-# LANGUAGE OverloadedStrings #-}
module Network.Mail where

import Type.Mirai.Update ( Update )
import Network.Mail.SMTP
    ( sendMailWithLoginSTARTTLS',
      simpleMail,
      Address(Address) )
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Mirai (getUserId, getUserRemark, getUserNick, getText, getMessageTime)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Util.Log (logWT'T, LogTag (Debug))
import Network.Mail.Mime (plainPart)

from = Address (Just "Nutr1t07 X") "nutr1t07@outlook.com"
to = Address (Just "Ne1s07 X") "ne1s07@outlook.com"

sendUpdateToEmail :: [Update] -> IO ()
sendUpdateToEmail [] = pure ()
sendUpdateToEmail upd@(head_upd:_) = sendMailWithLoginSTARTTLS' "smtp.office365.com" 587 "nutr1t07@outlook.com" "233Microsoft" mail
  where mail =
          simpleMail from [to] [] []
                     ("[Bot] messages from [" <> name <> "]")
                     [ plainPart mailContent ]
        mailContent = "from: " <> TL.fromStrict senderInfo <> "\n\n"
                    <> "text: \n" <> TL.fromStrict text <> "\n"
                    <> "origin: " <> TL.pack (show upd) <> "\n"
        text = mconcat $ genText (reverse upd)
        genText = fmap (\x -> "  " <> T.pack (show $ fromMaybe 0 $ getMessageTime x) <> "> "
                               <> fromMaybe "" (getText x) <> "\n")

        senderInfo = name <> " "  <> T.pack (maybe  "(No Id)" show (getUserId head_upd))
        remark = getUserRemark head_upd
        nick = getUserNick head_upd
        name = if remark == nick
                 then fromMaybe "(No Nick)" (getUserNick head_upd)
                 else fromMaybe "(No Remark)" (getUserRemark head_upd) <> " "
                   <> fromMaybe "(No Nick)" (getUserNick head_upd)