{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Gotten where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A hiding (Key)
import           Data.Maybe              (fromMaybe, listToMaybe)
import           Data.Text               (Text)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sql    (fromSqlKey)
import           Database.Persist.Sqlite (runSqlite)
import           Prelude                 hiding (id)
import           Snap

unzipMaybe :: Maybe (a,b) -> (Maybe a, Maybe b)
unzipMaybe Nothing       = (Nothing, Nothing)
unzipMaybe (Just (x, y)) = (Just x, Just y)

user :: Text -> [(a, Twits)] -> Maybe (a, Twits)
user usr = listToMaybe . filter by
  where by = (== usr) 
           . twitsName  
           . snd

idOrbdy :: ((Maybe a, Maybe Twits) -> c) -> Text -> [(a, Twits)] -> c
idOrbdy fos usr = fos . unzipMaybe . user usr

userKeyMb :: Text -> [(Key Twits, Twits)] -> Maybe Integer
userKeyMb usr = fmap (toInteger . fromSqlKey) 
              . idOrbdy fst usr

followedBy :: Text -> [(a, Twits)] -> [Integer]
followedBy usr  = fmap toInteger
                . fromMaybe []
                . fmap twitsFollow
                . idOrbdy snd usr

respBuilder :: [(Key Tweets, Tweets)] -> [(Key Twits, Twits)] -> MessageResps
respBuilder twts usrs = 
  MessageResps
    { responseMsgs = 
      (\(id, t) -> 
        MessageResp
          (tweetsUser_name t)
          (userKeyMb (tweetsUser_name t) usrs)
          (tweetsContent t)
          (toInteger <$> tweetsLikes t)
          Nothing
          (followedBy (tweetsUser_name t) usrs)
          (toInteger $ fromSqlKey id)
      ) <$> twts
    }

gotten :: Snap ()
gotten = do
  eTweets <- liftIO $ runSqlite "Twits.db" 
                    $ selectList [] [Desc TweetsCreated_at]
  eUsers  <- liftIO $ runSqlite "Twits.db" 
                    $ selectList [] [Desc TwitsName]
  let tweets   = (\(Entity id t)  -> (id, t))  <$> eTweets
      users    = (\(Entity uid u) -> (uid, u)) <$> eUsers
      response = respBuilder tweets users
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS (A.encode response)
