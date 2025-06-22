{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Common.Api where

import           Crypto.Hash              (SHA256 (SHA256), hashWith)
import           Data.Aeson               (FromJSON, ToJSON)
import qualified Data.ByteString.Internal as I
import           Data.Text                (Text, pack)
import           Data.Text.Encoding       (encodeUtf8)
import           Data.Time                (UTCTime)
import           GHC.Generics             (Generic)
import Data.Map (Map)

data AuthToken = AuthToken
  { authUserId :: UserName
  , authIssued :: UTCTime
  } deriving stock (Generic, Show)

instance ToJSON AuthToken
instance FromJSON AuthToken

data UserDetailsReq = UserDetailsReq
  { _username :: UserName
  , _password :: Text
  } deriving stock (Show, Generic)
instance FromJSON UserDetailsReq
instance ToJSON UserDetailsReq

data UserDetailsResp = UserDetailsResp
  { userDetailsMsg :: Msg
  } deriving stock (Show, Generic)
instance FromJSON UserDetailsResp
instance ToJSON UserDetailsResp

data MessageReq = MessageReq
  { reqUserName   :: UserName
  , userId        :: Integer
  , userInput     :: Msg
  , reqAuthUserId :: Text
  } deriving stock (Show, Eq, Generic)
instance ToJSON MessageReq
instance FromJSON MessageReq

type UserName = Text
type Msg = Text
data Like = Like deriving stock (Show, Eq, Generic)
instance ToJSON Like
instance FromJSON Like
data Reply = Replies (Maybe [MessageReq]) deriving stock (Show, Eq, Generic)
instance ToJSON Reply
instance FromJSON Reply
data Follow = Follow deriving stock (Show, Eq, Generic)
instance ToJSON Follow
instance FromJSON Follow

data MessageResp = MessageResp
  { resUserName :: UserName
  , resUserId   :: Maybe Integer
  , message     :: Msg
  , likes       :: [Integer]
  , replies     :: Maybe MessageResps
  , follows     :: [Integer]
  , msgId       :: Integer
  } deriving stock (Show, Eq, Generic)
instance ToJSON MessageResp
instance FromJSON MessageResp

data MessageResps = MessageResps
  { responseMsgs :: [MessageResp] -- look at this add likes, replies
  } deriving stock (Show, Eq, Generic)
instance ToJSON MessageResps
instance FromJSON MessageResps

data MessageReply = MessageReply
  { reply     :: Maybe MessageReq
  , like      :: Maybe Like
  , follow    :: Maybe Follow
  , parentId  :: (Maybe Integer)
  , replierId :: Integer
  } deriving stock (Show, Eq, Generic)
instance ToJSON MessageReply
instance FromJSON MessageReply

hashForSending :: Text -> I.ByteString
hashForSending password =
  encodeUtf8 $ pack $ show (hashWith SHA256 (encodeUtf8 password))

data MessageRespS = MessageRespS
  { resUserNameS :: UserName
  , resUserIdS   :: Maybe Integer
  , messageS     :: Msg
  , likesS       :: [Integer]
  , repliesS     :: Maybe MessageRespsS
  , followsS     :: [Integer]
  , createdS     :: UTCTime
  , msgIdS       :: Integer
  } deriving stock (Show, Eq, Generic)
instance ToJSON MessageRespS
instance FromJSON MessageRespS

data MessageRespsS = MessageRespsS 
  { responseMsgsS :: Map Integer MessageRespS
  } deriving stock (Show, Eq, Generic)
instance ToJSON MessageRespsS
instance FromJSON MessageRespsS
