{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Validate where

import           Common.Api
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             as A
import           Shared.Functions
import           Snap

auth :: Snap ()
auth = void validate

validate :: Snap (Maybe UserInfo)
validate = do
  mToken <- getCookie "session"
  case mToken of
    Nothing -> do modifyResponse $ setResponseCode 401
                  writeLBS "Not logged in "
                  pure Nothing
    Just token -> do
      mAuth <- do key <- liftIO getKey
                  pure $ verifyToken key (cookieValue token)
      jui <- case mAuth of
        Just (AuthToken name uid _) -> do let userInfo = UserInfo name uid
                                          writeLBS $ encode userInfo
                                          pure (Just userInfo)
        Nothing                     -> do modifyResponse $ setResponseCode 401
                                          pure Nothing
      pure (jui)
