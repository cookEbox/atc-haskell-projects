{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
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
{-# LANGUAGE DerivingStrategies    #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import qualified Data.ByteString.Internal as I
import           Data.Functor.Identity
import           Data.Text             (Text)

import Crypto.Hash (SHA256 (SHA256), hashWith)
import           Data.Text.Encoding      (encodeUtf8)
import           Obelisk.Route
import           Obelisk.Route.TH
import           Data.Text               (pack)

import           Data.Aeson            (FromJSON, ToJSON)
import           GHC.Generics          (Generic)

post :: Text
post = "post" 

get :: Text
get = "get" 

-- API request/response data type
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
  , userInput :: Text 
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

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Post :: BackendRoute ()
  BackendRoute_Get :: BackendRoute ()
  BackendRoute_Login :: BackendRoute ()
  BackendRoute_Signup :: BackendRoute ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_Login :: FrontendRoute ()
  FrontendRoute_Signup :: FrontendRoute ()

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
    BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
    BackendRoute_Post -> PathSegment post $ unitEncoder mempty 
    BackendRoute_Get -> PathSegment get $ unitEncoder mempty 
    BackendRoute_Login -> PathSegment "login" $ unitEncoder mempty 
    BackendRoute_Signup -> PathSegment "newuser" $ unitEncoder mempty 
  )
  (\case
    FrontendRoute_Login -> PathEnd $ unitEncoder mempty
    FrontendRoute_Signup -> PathSegment "signup" $ unitEncoder mempty
    FrontendRoute_Main -> PathSegment "main" $ unitEncoder mempty
  )

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
