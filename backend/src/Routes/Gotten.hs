{-# LANGUAGE OverloadedStrings #-}

module Routes.Gotten where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sqlite (runSqlite)
import           Snap

gotten :: Snap ()
gotten = do
  (eTweets) <- liftIO $ runSqlite "Twits.db" $ selectList [] [Desc TweetsCreated_at]
  let tweets = (\(Entity _ t) -> t) <$> eTweets
      response = MessageResp
                  { responseMsg = (\t -> (tweetsUser_name t, tweetsContent t)) <$> tweets
                  }
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS (A.encode response)
