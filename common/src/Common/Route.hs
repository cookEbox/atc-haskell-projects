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

import           Data.Functor.Identity
import           Data.Text             (Text)

import           Obelisk.Route
import           Obelisk.Route.TH

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

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Post :: BackendRoute ()
  BackendRoute_Get :: BackendRoute ()
  BackendRoute_Login :: BackendRoute ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_Login :: FrontendRoute ()

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
    BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
    BackendRoute_Post -> PathSegment post $ unitEncoder mempty 
    BackendRoute_Get -> PathSegment get $ unitEncoder mempty 
    BackendRoute_Login -> PathSegment "login" $ unitEncoder mempty 
  )
  (\case
    FrontendRoute_Login -> PathEnd $ unitEncoder mempty
    FrontendRoute_Main -> PathSegment "main" $ unitEncoder mempty
  )

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
