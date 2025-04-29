{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Posted where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import           Data.Maybe              (fromMaybe)
import           Data.Text               (pack)
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LE
import           Data.Text.Encoding      (encodeUtf8)
import           Data.Time.Clock         (getCurrentTime)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sqlite (runSqlite)
import           Shared.Functions
import           Snap


posted :: Snap ()
posted = do
  req <- getRequestBody
  case A.decode req of
    Just (MessageReq user reqMsg authToken) -> do
      encoded <- liftIO $ super_secret_DELETE
      authorised <- liftIO $ validateAuthToken authToken
      let authUser = authUserId <$> verifyToken (encodeUtf8 . pack $ encoded) authToken
      userName <- pure $ fromMaybe (pack " User") $ authUser
      isUser <- pure $ fromMaybe False $ (==) user <$> authUser
      if authorised && isUser
      then do
        utc <- liftIO getCurrentTime
        let newTweet = Tweets 1 user Nothing reqMsg utc
        tweetId <- liftIO $ runSqlite "Twits.db" $ insert newTweet
        let response = MessageResp
                        { responseMsg = [
                                          ( user, ( "Your input was: "
                                                  <> reqMsg
                                                  <> "\nYour Id is: "
                                                  <> (pack . show $ tweetId)
                                                  )
                                          )
                                        ]
                        }
        modifyResponse $ setHeader "Content-Type" "application/json"
        writeLBS (A.encode response)  -- Send JSON response to frontend
      else
        writeLBS $ "{\"error\": \"Invalid Authorisation Token for" <> (LE.encodeUtf8 . LT.fromStrict . pack $ show authorised) <> "\" }"  -- Send error response

    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeLBS "{\"error\": \"Invalid JSON\"}"  -- Send error response
