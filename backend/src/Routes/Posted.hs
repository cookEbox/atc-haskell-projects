{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Posted where

import           Common.Api
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import           Data.Text               (Text)
import           Data.Time.Clock         (getCurrentTime)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sqlite (ConnectionPool, runSqlPool)
import           Routes.Validate
import           Shared.Functions
import           Snap

decodeAndRespond :: ConnectionPool -> UserInfo -> Maybe MessageReq -> Snap ()
decodeAndRespond _ _ Nothing = do
  modifyResponse $ setResponseStatus 400 "Bad Request"
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . A.encode $ A.object ["error" .= ("Invalid JSON" :: Text)]
decodeAndRespond pool userInfo (Just (MessageReq user uid reqMsg)) = do
  let isUser = uid == uiId userInfo
  if isUser
  then do
    utc <- liftIO getCurrentTime
    let newTweet = Tweets user (intToSqlKey uid) [] [] reqMsg utc utc
    void $ liftIO $ runSqlPool (insert newTweet) pool
  else do
    let errMsg = "Invalid Authorisation Token for " <> uiName userInfo
    writeLBS . A.encode $ A.object ["error" .= (errMsg :: Text)]

posted :: ConnectionPool -> Snap ()
posted pool = do
  req <- getRequestBody
  authorised <- validate
  case authorised of
    Just userInfo -> decodeAndRespond pool userInfo $ A.decode req
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeLBS . A.encode $ A.object ["error" .= ("Not Logged In" :: Text)]
