{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.WebSocket where

import           Common.Api
import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, evalStateT, get, put)
import           Data.Aeson                 as A hiding (Key)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime, NominalDiffTime,
                                             diffUTCTime, getCurrentTime)
import           Database.DB
import           Database.Persist           hiding (Add, count, get)
import           Database.Persist.Sql       (fromSqlKey)
import           Database.Persist.Sqlite    (ConnectionPool, runSqlPool)
import           Network.WebSockets
import           Network.WebSockets.Snap
import           Prelude                    hiding (id)
import           Snap
import           System.Directory           (getModificationTime)

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

getDelta :: ConnectionPool -> UTCTime -> IO (UTCTime, [Entity Tweets])
getDelta pool lastTime = do
  tweets <- runSqlPool (selectList [TweetsUpdated_at >=. lastTime] [Desc TweetsCreated_at]) pool
  nowish <- runSqlPool (selectFirst [] [Desc TweetsUpdated_at, LimitTo 1]) pool
  let now = case nowish of 
            Just (Entity _ tweet) -> tweetsUpdated_at tweet
            Nothing               -> lastTime
  pure (now, tweets)

entityToPair :: Entity b -> (Key b, b)
entityToPair (Entity k v) = (k, v)

poolLoop :: ConnectionPool -> Connection -> StateT UTCTime IO () 
poolLoop pool conn = forever $ do
  liftIO $ threadDelay (500 * 1000)  -- 500 ms
  changedDb  <- liftIO $ wasRecentlyModified "Twits.db"     0.5
  changedWAL <- liftIO $ wasRecentlyModified "Twits.db-wal" 0.5
  when (changedDb || changedWAL) $ do
    lastTime <- get
    (new, tweets') <- liftIO $ getDelta pool lastTime
    users' <- liftIO $ runSqlPool (selectList [] [Desc TwitsName])    pool
    let response' = respBuilder (map entityToPair tweets') (map entityToPair users')
    liftIO $ sendTextData conn (A.encode response')
    put new

myWebSocketApp :: ConnectionPool -> ServerApp
myWebSocketApp pool pending = do
  eTweets <- liftIO $ runSqlPool (selectList [] [Desc TweetsCreated_at]) pool
  eUsers  <- liftIO $ runSqlPool (selectList [] [Desc TwitsName]) pool
  let tweets   = entityToPair <$> eTweets
      users    = entityToPair <$> eUsers
      response = respBuilder tweets users
  conn <- acceptRequest pending
  sendTextData conn (A.encode response)
  now <- getCurrentTime
  liftIO $ evalStateT (poolLoop pool conn) now

websocket :: ConnectionPool -> Snap ()
websocket pool = runWebSocketsSnap $ myWebSocketApp pool
