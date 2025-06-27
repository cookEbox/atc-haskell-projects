{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Routes.WebSocket where

import           Common.Api
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.MVar    (MVar, modifyMVar_, newMVar,
                                             readMVar)
import           Control.Monad              (forever, void)
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
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import           Database.DB
import           Database.Persist           hiding (Add, count, get)
import qualified Database.Persist           as P (get)
import           Database.Persist.Sql       (SqlBackend, fromSqlKey, toSqlKey)
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
                . fmap twitsFollowers
                . idOrbdy snd usr

respBuilder :: [(Key Tweets, Tweets)] 
            -> [(Key Twits, Twits)] 
            -> PatchOrReplace 
            -> MessageRespsS
respBuilder twts usrs por =
  MessageRespsS por $ M.fromList $
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

fetchTweetsDelta :: ConnectionPool
                 -> UTCTime
                 -> [Filter Tweets]
                 -> IO (UTCTime, [Entity Tweets])
fetchTweetsDelta pool lastTime filt = runDB pool $ do
  let filters = (TweetsUpdated_at >. lastTime) : filt
  tws    <- selectList filters [Desc TweetsCreated_at]
  latest <- selectFirst [(TweetsUpdated_at >. lastTime)] 
                        [Desc TweetsUpdated_at, LimitTo 1]
  let now = maybe lastTime (tweetsUpdated_at . entityVal) latest
  pure (now, tws)

getTweetDelta :: ConnectionPool 
              -> UTCTime 
              -> ClientMsg 
              -> IO [(UTCTime, [Entity Tweets])]
getTweetDelta pool lastTime All 
  = sequence . (:[]) $ fetchTweetsDelta pool lastTime []
getTweetDelta pool lastTime (UserMsgs uid)
  = sequence . (:[]) $ fetchTweetsDelta pool lastTime 
                       [TweetsUser_id ==. fromIntegral uid]
getTweetDelta pool lastTime (Following uid) = do
  fsm <- runDB pool $ fmap twitsFollowing 
                   <$> P.get (toSqlKey $ fromIntegral uid)
  let fs      = fromMaybe [] fsm
      fetch f = fetchTweetsDelta pool lastTime [TweetsUser_id ==. f]
  sequence $ fetch <$> fs

getUserDelta :: ConnectionPool -> UTCTime -> IO (UTCTime, [Entity Twits])
getUserDelta pool lastTime = do
  (users, mLatest) <- runDB pool $ do
    usrs   <- selectList [TwitsUpdated_at >. lastTime] [Desc TwitsName]
    latest <- selectFirst [] [Desc TwitsUpdated_at, LimitTo 1]
    pure (usrs, latest)
  let now = maybe lastTime (twitsUpdated_at . entityVal) mLatest
  pure (now, users)

entityToPair :: Entity b -> (Key b, b)
entityToPair (Entity k v) = (k, v)

returnOrGrabAll :: (Foldable t, MonadIO f) =>
                   ConnectionPool
                   -> [Entity Tweets]
                   -> t a
                   -> ClientMsg
                   -> f ([Entity Tweets], PatchOrReplace)
returnOrGrabAll pool ts us sub 
  | not (null ts) = pure (ts, Patch)
  | not (null us) = (,Replace) <$> grabAll pool sub
  | otherwise     = pure ([], Patch) 

sendToClient :: ConnectionPool
             -> Connection
             -> UTCTime
             -> [Entity Tweets]
             -> PatchOrReplace
             -> StateT UTCTime IO ()
sendToClient pool conn newTime twtsToSend por 
  | por == Patch && null twtsToSend = pure ()
  | otherwise = do
      allUsers <- runDB pool (selectList [] [Desc TwitsName])
      let resp = respBuilder
                   (map entityToPair twtsToSend)
                   (map entityToPair allUsers)
                   por
      liftIO $ sendTextData conn (A.encode resp)
      put newTime

flipListTuple :: [(a, [b])] -> ([a], [b])
flipListTuple lst = (fmap fst lst, concat $ fmap snd lst)

lastUpdate :: Ord a => [a] -> a -> a
lastUpdate []    uTime = uTime 
lastUpdate tTime uTime = max (minimum tTime) uTime

poolLoop :: ConnectionPool 
         -> Connection 
         -> MVar ClientMsg 
         -> StateT UTCTime IO ()
poolLoop pool conn subVar = forever $ do
  liftIO $ threadDelay (100 * 1000)  -- 100 ms
  lastTime <- get
  sub <- liftIO $ readMVar subVar
  tweetDelta        <- liftIO $ getTweetDelta pool lastTime sub
  (uTime, newUsers) <- liftIO $ getUserDelta pool lastTime
  let (tTime, newTweets) = flipListTuple tweetDelta
      newTime = lastUpdate tTime uTime 
  case (newTweets, newUsers) of
    ([], [])   -> pure ()
    (ts, us)   -> do
      (twtsToSend, por) <- returnOrGrabAll pool ts us sub
      sendToClient pool conn newTime twtsToSend por

initialAllDb :: MonadIO m
             => ConnectionPool
             -> m ([Entity Tweets], [Entity Twits])
initialAllDb pool = runDB pool $ do
  twts <- selectList [] [Desc TweetsCreated_at]
  usrs <- selectList [] [Desc TwitsName]
  pure (twts, usrs)

initialUserDb :: MonadIO m
              => ConnectionPool
              -> Integer
              -> m ([Entity Tweets], [Entity Twits])
initialUserDb pool uid = runDB pool $ do
  let twitsKey = toSqlKey (fromIntegral uid)
  twts <- selectList [TweetsUser_id ==. fromIntegral uid] 
                     [Desc TweetsCreated_at]
  usrs <- selectList [TwitsId ==. twitsKey] []
  pure (twts, usrs)

initialFollowersDb :: MonadIO m
                   => ConnectionPool
                   -> Integer
                   -> m ([Entity Tweets], [Entity Twits])
initialFollowersDb pool uid = runDB pool $ do
  let twitsKey = toSqlKey (fromIntegral uid)
  fsm <- runDB pool $ fmap twitsFollowing <$> P.get twitsKey
  let fs = fromMaybe [] fsm
      fetch f = fetchTweetsDelta 
                  pool 
                  (posixSecondsToUTCTime 0) 
                  [TweetsUser_id ==. f]
  tweets <- liftIO . sequence $ fetch <$> fs
  let twts = concat $ (\t -> fmap snd t) tweets
  usrs <- selectList [] [Desc TwitsName]
  pure (twts, usrs)

grabAll :: MonadIO m => ConnectionPool -> ClientMsg -> m [Entity Tweets]
grabAll pool (Following uid) = do (twts, _) <- initialFollowersDb pool uid
                                  pure twts
grabAll pool _               = do (twts, _) <- initialAllDb pool
                                  pure twts


mostRecentUpdateTime :: UTCTime -> [Entity Tweets] -> [Entity Twits] -> UTCTime
mostRecentUpdateTime n []                []                = n
mostRecentUpdateTime _ (Entity _ t1 : _) []                = tweetsUpdated_at t1
mostRecentUpdateTime _ []                (Entity _ t2 : _) = twitsUpdated_at t2
mostRecentUpdateTime _ (Entity _ t1 : _) (Entity _ t2 : _)
  = max (tweetsUpdated_at t1) (twitsUpdated_at t2)

initial :: ConnectionPool -> IO (MessageRespsS, UTCTime)
initial pool = do
  now <- getCurrentTime
  (eTweets, eUsers) <- initialAllDb pool
  let tweets      = entityToPair <$> eTweets
      users       = entityToPair <$> eUsers
      response    = respBuilder tweets users Replace
      initialTime = mostRecentUpdateTime now eTweets eUsers
  pure (response, initialTime)

sendAll :: ConnectionPool -> Connection -> ClientMsg -> IO ()
sendAll pool conn All = do
  (mainTweets, _) <- initial pool
  sendTextData conn (A.encode mainTweets)
sendAll pool conn (UserMsgs uid) = do 
  (uTweets, uUser) <- initialUserDb pool uid
  let tweets      = entityToPair <$> uTweets
      users       = entityToPair <$> uUser
      response    = respBuilder tweets users Replace
  sendTextData conn (A.encode response)
sendAll pool conn (Following uid) = do 
  (fTweets, fUsers) <- initialFollowersDb pool uid
  let tweets      = entityToPair <$> fTweets
      users       = entityToPair <$> fUsers
      response    = respBuilder tweets users Replace
  sendTextData conn (A.encode response)

runWebSocket :: ConnectionPool -> ServerApp
runWebSocket pool pending = do
  conn <- acceptRequest pending
  subVar <- newMVar All
  void $ forkIO . forever $ do
    msg <- receiveData conn
    case A.decode msg of
      Just newSub -> do 
        modifyMVar_ subVar (const $ pure newSub)
        sendAll pool conn newSub
      Nothing     -> pure ()
  (initialResp, initialLastTime) <- initial pool
  sendTextData conn (A.encode initialResp)
  liftIO $ evalStateT (poolLoop pool conn subVar) initialLastTime

websocket :: ConnectionPool -> Snap ()
websocket = runWebSocketsSnap . runWebSocket
