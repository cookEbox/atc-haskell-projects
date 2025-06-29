{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Posted where

import           Common.Api
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LE
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
  writeLBS "{\"error\": \"Invalid JSON\"}"
decodeAndRespond pool userInfo (Just (MessageReq user uid reqMsg)) = do
  let isUser = uid == uiId userInfo
  if isUser
  then do
    utc <- liftIO getCurrentTime
    let newTweet = Tweets user (intToSqlKey uid) [] [] reqMsg utc utc
    void $ liftIO $ runSqlPool (insert newTweet) pool
  else
    writeLBS $ "{\"error\": \"Invalid Authorisation Token for" 
            <> (LE.encodeUtf8 . LT.fromStrict $ uiName userInfo) 
            <> "\" }"

posted :: ConnectionPool -> Snap ()
posted pool = do
  req <- getRequestBody
  authorised <- validate
  case authorised of 
    Just userInfo -> decodeAndRespond pool userInfo $ A.decode req
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeLBS "{\"error\": \"Not Logged In\"}"
