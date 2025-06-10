{-# LANGUAGE OverloadedStrings #-}

module Routes.Gotten where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sqlite (runSqlite)
import           Database.Persist.Sql    (fromSqlKey)
import           Snap
import Prelude hiding (id)

gotten :: Snap ()
gotten = do
  eTweets <- liftIO $ runSqlite "Twits.db" $ selectList [] [Desc TweetsCreated_at]
  let tweets = (\(Entity id t) -> (id, t)) <$> eTweets
      response = MessageResps
                  { responseMsgs = (\(id, t) -> MessageResp 
                                                (tweetsUser_name t) 
                                                (tweetsContent t)
                                                (toInteger $ length $ tweetsLikes t)
                                                Nothing 
                                                (toInteger $ fromSqlKey id)
                                   ) <$> tweets
                  }
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS (A.encode response)
