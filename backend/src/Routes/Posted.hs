{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Posted where

import           Common.Api
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import           Data.Maybe              (fromMaybe)
import           Data.Text               (pack)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LE
import           Data.Time.Clock         (getCurrentTime)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sqlite (runSqlPool, ConnectionPool)
import           Shared.Functions
import           Snap


posted :: ConnectionPool -> Snap ()
posted pool = do
  req <- getRequestBody
  case A.decode req of
    Just (MessageReq user _ reqMsg authToken) -> do
      encoded <- liftIO $ super_secret_DELETE
      authorised <- liftIO $ validateAuthToken authToken
      let authUser = authUserId <$> verifyToken (encodeUtf8 . pack $ encoded) authToken
      userName <- pure $ fromMaybe (pack " User") $ authUser
      isUser <- pure $ fromMaybe False $ (==) user <$> authUser
      if authorised && isUser
      then do
        utc <- liftIO getCurrentTime
        let newTweet = Tweets user [] [] reqMsg utc utc
        void $ liftIO $ runSqlPool (insert newTweet) pool
      else
        writeLBS $ "{\"error\": \"Invalid Authorisation Token for" <> (LE.encodeUtf8 . LT.fromStrict $ userName) <> "\" }"

    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeLBS "{\"error\": \"Invalid JSON\"}"
