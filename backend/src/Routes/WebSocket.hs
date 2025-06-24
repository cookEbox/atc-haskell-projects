{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.WebSocket where

import           Common.Api
import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.State.Strict (StateT, evalStateT, get, put)
import           Data.Aeson                 as A hiding (Key)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Data.Pool                  (Pool)
import           Data.Text                  (Text)
import           Data.Time.Clock            (NominalDiffTime, UTCTime,
                                             diffUTCTime, getCurrentTime)
import           Database.DB
import           Database.Persist           hiding (Add, count, get)
import           Database.Persist.Sql       (SqlBackend, fromSqlKey)
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
         (toInteger $ fromSqlKey id)
      )
    ) <$> twts

runDB :: (MonadIO m, BackendCompatible SqlBackend backend)
      => Pool backend
      -> ReaderT backend IO a
      -> m a
runDB pool action = liftIO $ runSqlPool action pool

getTweetDelta :: ConnectionPool -> UTCTime -> IO (UTCTime, [Entity Tweets])
getTweetDelta pool lastTime = do
  (tweets, mLatest) <- runDB pool $ do
    tws    <- selectList [TweetsUpdated_at >=. lastTime] [Desc TweetsCreated_at]
    latest <- selectFirst [] [Desc TweetsUpdated_at, LimitTo 1]
    pure (tws, latest)
  let now = maybe lastTime (tweetsUpdated_at . entityVal) mLatest
  pure (now, tweets)

getUserDelta :: ConnectionPool -> UTCTime -> IO (UTCTime, [Entity Twits])
getUserDelta pool lastTime = do
  (users, mLatest) <- runDB pool $ do
    usrs   <- selectList [TwitsUpdated_at >=. lastTime] [Desc TwitsName]
    latest <- selectFirst [] [Desc TwitsUpdated_at, LimitTo 1]
    pure (usrs, latest)
  let now = maybe lastTime (twitsUpdated_at . entityVal) mLatest
  pure (now, users)

entityToPair :: Entity b -> (Key b, b)
entityToPair (Entity k v) = (k, v)

poolLoop :: ConnectionPool -> Connection -> StateT UTCTime IO ()
poolLoop pool conn = forever $ do
  liftIO $ threadDelay (500 * 1000)  -- 500 ms
  lastTime <- get
  (tTime, newTweets) <- liftIO $ getTweetDelta pool lastTime
  (uTime, newUsers)  <- liftIO $ getUserDelta pool lastTime
  let newTime = max tTime uTime
  when (not (null newTweets) || not (null newUsers)) $ do
    twtsToSend <- if not (null newTweets)
                  then pure newTweets 
                  else runDB pool (selectList [] [Desc TweetsCreated_at])
    users <- runDB pool (selectList [] [Desc TwitsName])
    let resp = respBuilder 
                (map entityToPair twtsToSend) 
                (map entityToPair users)
    liftIO $ sendTextData conn (A.encode resp)
    put newTime

runWebSocket :: ConnectionPool -> ServerApp
runWebSocket pool pending = do
  (eTweets, eUsers) <- runDB pool $ do
    twts <- selectList [] [Desc TweetsCreated_at]
    usrs <- selectList [] [Desc TwitsName]
    pure (twts, usrs)
  let tweets   = entityToPair <$> eTweets
      users    = entityToPair <$> eUsers
      response = respBuilder tweets users
  conn <- acceptRequest pending
  sendTextData conn (A.encode response)
  now <- getCurrentTime
  let initialLastTime = case eTweets of
          (Entity _ t : _) -> tweetsUpdated_at t
          []               -> now
  liftIO $ evalStateT (poolLoop pool conn) initialLastTime

websocket :: ConnectionPool -> Snap ()
websocket = runWebSocketsSnap . runWebSocket 
