{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.User where

import Type.Mirai.Update
import Data.Text
import Data.IORef (newIORef, IORef, readIORef, modifyIORef, writeIORef)
import Control.Monad (when, unless)
import GHC.IORef (readIORef)
import Data.Maybe (isJust)
import Util.Log (logErr)
import Codec.Serialise (serialise, deserialiseOrFail, DeserialiseFailure (DeserialiseFailure))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Codec.Serialise.Class (Serialise)
import Control.Exception.Base (try, SomeException (SomeException))
import Data.Either (fromRight)

type UserGroup = IORef [User]


savePath :: [Char]
savePath = "savedUser.dat"

saveUserGroup :: UserGroup -> IO ()
saveUserGroup grp = do
  grp' <- readIORef grp
  BL.writeFile savePath $ serialise grp'

readUserGroup :: IO (Maybe UserGroup)
readUserGroup = do
  raw <- fromRight "BL.empty" <$> (try (BS.readFile savePath) :: IO (Either SomeException BS.ByteString))
  case deserialiseOrFail (BL.fromStrict raw) of
    Right x -> Just <$> newIORef x
    Left err -> logErr "reading user group from local file" (show err) >> pure Nothing


emptyUserGroup :: IO UserGroup
emptyUserGroup = newIORef []

createUser :: UserGroup -> Integer -> IO User
createUser grp userid' = addUser grp newUser
  where newUser = User userid' Greeting 0 [] empty [] []

addUser :: UserGroup -> User -> IO User
addUser grp user = do
  exist <- isJust <$> fetchUser' grp (userId user)
  when exist $ logErr "adding user" "user already exist"
  unless exist $ modifyIORef grp (user:)
  pure user

replaceUser :: UserGroup -> User -> IO User
replaceUser grp user@User{userId = targetUserId} = do
  exist <- isJust <$> fetchUser' grp targetUserId
  unless exist $ logErr "replace user" "user not exist"
  when exist $ modifyIORef grp replace'
  pure user
  where
    replace' [] = []
    replace' (x:xs) = if userId x == targetUserId then user : xs else x : replace' xs

fetchUser :: UserGroup -> Integer -> IO User
fetchUser grp userId' = do
    usr' <- fetchUser' grp userId'
    case usr' of
      Just x -> pure x
      Nothing -> createUser grp userId'

fetchUser' :: UserGroup -> Integer -> IO (Maybe User)
fetchUser' grp userid' = do
  grp' <- readIORef grp
  pure $ findFirst grp' userid'
  where
    findFirst [] _ = Nothing 
    findFirst (usr:usrs) targetId =
      if userId usr == targetId
        then Just usr 
        else findFirst usrs targetId



data User = User {
    userId      :: Integer 
  , state       :: State
  , stage       :: Integer 
  , recordedMsg :: [Update]
  , remark      :: Text
  , flag        :: [Flag]
  , replied     :: [Text]
} deriving (Generic)
instance Serialise User

data Flag  = TooMuchWord | Editing Int
  deriving (Eq, Generic)
instance Serialise Flag

data State = Greeting | Recording | Idle | Searching
  deriving (Eq, Generic)
instance Serialise State

type RepliedTable = IORef [(UserID, [Text])]
type UserID = Integer 

emptyRepliedTable :: IO RepliedTable
emptyRepliedTable = newIORef []

markReplied :: RepliedTable -> Text -> UserID -> IO ()
markReplied table' txt userId= do
  table <- readIORef table'
  writeIORef table' (mod table)
  where mod [] = [(userId, [txt])]
        mod (x@(usrId', txts'):xs) = if usrId' == userId
            then (usrId', txt:txts'):xs
            else x : mod xs

isReplied :: RepliedTable -> Text -> UserID -> IO Bool
isReplied table' txt userId = do
  table <- readIORef table'
  pure $ case lookup userId table of
           Just x -> txt `elem` x
           Nothing -> False
  