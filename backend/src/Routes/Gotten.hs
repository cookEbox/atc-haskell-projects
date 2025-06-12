{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Gotten where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import           Data.Maybe              (fromMaybe, listToMaybe)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sql    (fromSqlKey)
import           Database.Persist.Sqlite (runSqlite)
import           Prelude                 hiding (id)
import           Snap

unzipMaybe :: Maybe (a,b) -> (Maybe a, Maybe b)
unzipMaybe Nothing = (Nothing, Nothing)
unzipMaybe (Just (x, y)) = (Just x, Just y)

gotten :: Snap ()
gotten = do
  eTweets <- liftIO $ runSqlite "Twits.db" $ selectList [] [Desc TweetsCreated_at]
  eUsers <- liftIO $ runSqlite "Twits.db" $ selectList [] [Desc TwitsName]
  let tweets = (\(Entity id t) -> (id, t)) <$> eTweets
      users2 = (\(Entity uid u) -> (uid, u)) <$> eUsers
      user2 usr = listToMaybe (filter ((\u -> twitsName u == usr) . snd) users2) 
      keyFollsMb usr = unzipMaybe $ user2 usr 
      followedBy2 usr = toInteger <$> (fromMaybe [] $ twitsFollow <$> (snd $ keyFollsMb usr)) 
      userKeyMb usr = toInteger . fromSqlKey <$> (fst $ keyFollsMb usr) 

      response   = MessageResps
                    { responseMsgs
                      = (\(id, t) -> MessageResp
                                     (tweetsUser_name t)
                                     (userKeyMb $ tweetsUser_name t)
                                     (tweetsContent t)
                                     (toInteger <$> tweetsLikes t)
                                     Nothing
                                     (followedBy2 $ tweetsUser_name t)
                                     (toInteger $ fromSqlKey id)
                        ) <$> tweets
                    }
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS (A.encode response)
