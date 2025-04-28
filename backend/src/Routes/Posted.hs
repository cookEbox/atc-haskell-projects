{-# LANGUAGE OverloadedStrings #-}

module Routes.Posted where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import           Data.Text               (pack)
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
      authorised <- liftIO $ validateAuthToken authToken 
      if authorised 
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
        writeLBS "{\"error\": \"Invalid Authorisation Token\"}"  -- Send error response

    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeLBS "{\"error\": \"Invalid JSON\"}"  -- Send error response
