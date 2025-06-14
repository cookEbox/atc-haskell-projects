{-# LANGUAGE OverloadedStrings #-}

module Routes.Signup where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sqlite (runSqlPool, ConnectionPool)
import           Shared.Functions
import           Snap

signup :: ConnectionPool -> Snap ()
signup pool = do
  req <- getRequestBody
  case A.decode req of
    Just (UserDetailsReq username password) -> do
      maybeUser <- liftIO $ runSqlPool (getBy (UniqueTwit username)) pool
      case maybeUser of
        Just (Entity _ _) -> do
          modifyResponse $ setResponseStatus 401 "Unauthorized"
          writeLBS "{\"error\": \"User already exists\"}"
        Nothing -> do
          liftIO $ storeUser username password
          writeLBS (A.encode $ UserDetailsResp "Success")
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      writeLBS "{\"error\": \"Invalid JSON\"}"

