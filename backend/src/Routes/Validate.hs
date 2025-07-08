{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Validate where

import           Common.Api
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             as A
import           Shared.Functions
import           Snap

auth :: Snap ()
auth = do
  mUser <- validate
  case mUser of
    Just userInfo -> do
      modifyResponse $ setResponseStatus 200 "OK"
                     . setHeader "Content-Type" "application/json"
      writeLBS (encode userInfo)
    Nothing -> do
      modifyResponse $ setResponseStatus 401 "Unauthorized"
                     . setHeader "Content-Type" "text/plain"
      writeLBS "Not logged in"

validate :: Snap (Maybe UserInfo)
validate = do
  mToken <- getCookie "session"
  case mToken of
    Nothing    -> pure Nothing
    Just token -> do
      key   <- liftIO getKey
      let mAuth = verifyToken key (cookieValue token)
      pure $ (\(AuthToken name uid _) -> UserInfo name uid) <$> mAuth
