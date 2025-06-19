{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.WebSocket where

import           Common.Api
import           Control.Concurrent      (threadDelay)
import           Control.Monad           (when)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A hiding (Key)
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe, listToMaybe)
import           Data.Text               (Text)
import           Data.Time.Clock         (NominalDiffTime, diffUTCTime,
                                          getCurrentTime)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sql    (fromSqlKey)
import           Database.Persist.Sqlite (ConnectionPool, runSqlPool)
import           Network.WebSockets
import           Network.WebSockets.Snap
import           Prelude                 hiding (id)
import           Snap
import           System.Directory        (getModificationTime)

wasRecentlyModified :: FilePath -> NominalDiffTime -> IO Bool
wasRecentlyModified dbPath threshold = do
  modTime <- getModificationTime dbPath
  now     <- getCurrentTime
  let delta = diffUTCTime now modTime
  return (delta < threshold)

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

respBuilder :: [(Key Tweets, Tweets)] -> [(Key Twits, Twits)] -> MessageRespsS
respBuilder twts usrs =
  MessageRespsS $ M.fromList $
    (\(id, t) ->
      ((toInteger $ fromSqlKey id),
         MessageRespS
         (tweetsUser_name t)
         (userKeyMb (tweetsUser_name t) usrs)
         (tweetsContent t)
         (toInteger <$> tweetsLikes t)
         Nothing
         (followedBy (tweetsUser_name t) usrs)
         (tweetsCreated_at t)
      )
    ) <$> twts

websocket :: ConnectionPool -> Snap ()
websocket pool = runWebSocketsSnap $ myWebSocketApp pool

myWebSocketApp :: ConnectionPool -> ServerApp
myWebSocketApp pool pending = do
  eTweets <- liftIO $ runSqlPool (selectList [] [Desc TweetsCreated_at]) pool
  eUsers  <- liftIO $ runSqlPool (selectList [] [Desc TwitsName]) pool
  let tweets   = (\(Entity id t)  -> (id, t))  <$> eTweets
      users    = (\(Entity uid u) -> (uid, u)) <$> eUsers
      response = respBuilder tweets users
  conn <- acceptRequest pending
  sendTextData conn (A.encode response)
  let loop = do
        threadDelay (500 * 1000)  -- 500 ms
        changed <- wasRecentlyModified "mydatabase.sqlite" 0.5
        when changed $ do
          tweets' <- runSqlPool (selectList [] [Desc TweetsCreated_at]) pool
          users' <- runSqlPool (selectList [] [Desc TwitsName])    pool
          let response' = respBuilder (map entityToPair tweets') (map entityToPair users')
          sendTextData conn (BL.toStrict $ A.encode response')
        loop
  loop
  where
    entityToPair (Entity k v) = (k, v)
