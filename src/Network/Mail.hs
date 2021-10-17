{-# LANGUAGE OverloadedStrings #-}
module Network.Mail where

import Type.Mirai.Update
import Network.Mail.SMTP

from = Address (Just "Nutr1t07 X") "nutr1t07@outlook.com"
to = Address (Just "Ne1s07 X") "ne1s07@outlook.com"

-- fromUpdate upd = simpleMail from [to] [] [] "[Bot] " ([Part])