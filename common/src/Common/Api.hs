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

import qualified Data.ByteString.Internal as I
import           Data.Text                (Text)

import           Crypto.Hash              (SHA256 (SHA256), hashWith)
import           Data.Text                (pack)
import           Data.Text.Encoding       (encodeUtf8)
import           Data.Time                (UTCTime)

import           Data.Aeson               (FromJSON, ToJSON)
import           GHC.Generics             (Generic)

post :: Text
post = "post"

get :: Text
get = "get"

data AuthToken = AuthToken
  { authUserId :: Text
  , authIssued :: UTCTime
  } deriving stock (Generic, Show)

instance ToJSON AuthToken
instance FromJSON AuthToken

data LoginReq = LoginReq
  { loginUsername :: Text
  , loginPassword :: Text
  } deriving stock (Show, Generic)
instance FromJSON LoginReq
instance ToJSON LoginReq

data LoginResp = LoginResp
  { loginMessage :: Text
  } deriving stock (Show, Generic)
instance FromJSON LoginResp
instance ToJSON LoginResp

data MessageReq = MessageReq
  { reqUserName :: Text
  , userInput   :: Text
  , reqAuthUserId :: Text
  } deriving stock (Show, Eq, Generic)
instance ToJSON MessageReq
instance FromJSON MessageReq

type UserName = Text
type Msg = Text

data MessageResp = MessageResp
  { responseMsg :: [(UserName, Msg)]
  } deriving stock (Show, Eq, Generic)
instance ToJSON MessageResp
instance FromJSON MessageResp

hashForSending :: Text -> I.ByteString
hashForSending password =
  encodeUtf8 $ pack $ show (hashWith SHA256 (encodeUtf8 password))
