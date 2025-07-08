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
module Common.Route where

import           Data.Functor.Identity
import           Data.Text                (Text)

import           Obelisk.Route
import           Obelisk.Route.TH

data BackendRoute :: * -> * where
  BackendRoute_Missing   :: BackendRoute ()
  BackendRoute_Post      :: BackendRoute ()
  BackendRoute_Login     :: BackendRoute ()
  BackendRoute_Logout    :: BackendRoute ()
  BackendRoute_Signup    :: BackendRoute ()
  BackendRoute_Update    :: BackendRoute ()
  BackendRoute_WebSocket :: BackendRoute ()
  BackendRoute_Auth      :: BackendRoute ()
  BackendRoute_Profile   :: BackendRoute ()
  BackendRoute_ProfileUp :: BackendRoute ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main      :: FrontendRoute ()
  FrontendRoute_Login     :: FrontendRoute ()
  FrontendRoute_Signup    :: FrontendRoute ()
  FrontendRoute_Profile   :: FrontendRoute ()
  FrontendRoute_ProfileUp :: FrontendRoute ()

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
    BackendRoute_Missing   -> PathSegment "smissing"   $ unitEncoder mempty
    BackendRoute_Post      -> PathSegment "post"       $ unitEncoder mempty
    BackendRoute_Login     -> PathSegment "slogin"     $ unitEncoder mempty
    BackendRoute_Logout    -> PathSegment "slogout"    $ unitEncoder mempty
    BackendRoute_Signup    -> PathSegment "ssignup"    $ unitEncoder mempty
    BackendRoute_Update    -> PathSegment "supdate"    $ unitEncoder mempty
    BackendRoute_WebSocket -> PathSegment "websocket"  $ unitEncoder mempty
    BackendRoute_Auth      -> PathSegment "auth"       $ unitEncoder mempty
    BackendRoute_Profile   -> PathSegment "sprofile"   $ unitEncoder mempty
    BackendRoute_ProfileUp -> PathSegment "sprofileup" $ unitEncoder mempty
  )
  (\case
    FrontendRoute_Login     -> PathSegment "login"     $ unitEncoder mempty
    FrontendRoute_Signup    -> PathSegment "signup"    $ unitEncoder mempty
    FrontendRoute_Profile   -> PathSegment "profile"   $ unitEncoder mempty
    FrontendRoute_ProfileUp -> PathSegment "profileup" $ unitEncoder mempty
    FrontendRoute_Main      -> PathEnd                 $ unitEncoder mempty
  )

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
