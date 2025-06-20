{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Update where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A hiding (Key)
import qualified Data.List               as L (delete, nub)
import           Data.Time.Clock         (getCurrentTime)
import           Database.DB
import           Database.Persist        as P hiding (Add, count)
import           Database.Persist.Sql    (toSqlKey)
import           Database.Persist.Sqlite (ConnectionPool, runSqlPool)
import           Prelude                 hiding (id)
import           Shared.Functions
import           Snap

updateMessageLikes :: ConnectionPool -> Integer -> Integer -> IO ()
updateMessageLikes pool key rid = do
  eTweets <- liftIO $ runSqlPool (selectList [] [Desc TweetsCreated_at]) pool
  utc <- liftIO getCurrentTime
  let tweets = (\(Entity id t) -> (id, t)) <$> eTweets
      keyid  = toSqlKey $ fromInteger key
      rid64  = fromInteger rid
      tweet  = head . filter (\id -> fst id == keyid)
      toggle lst = if elem rid64 lst then L.delete rid64 lst else rid64 : lst
      incLikes  = L.nub . toggle . tweetsLikes . snd . tweet
  runSqlPool (P.update keyid [TweetsLikes =. incLikes tweets, TweetsUpdated_at =. utc]) pool

updateMessageFollows :: ConnectionPool -> Integer -> Integer -> IO ()
updateMessageFollows pool key rid = do
  eUsers <- liftIO $ runSqlPool (selectList [] [Desc TwitsName]) pool
  utc <- liftIO getCurrentTime
  let users  = (\(Entity id u) -> (id, u)) <$> eUsers
      keyid  = toSqlKey $ fromInteger key
      rid64  = fromInteger rid
      user   = head . filter (\id -> fst id == keyid)
      toggle lst = if elem rid64 lst then L.delete rid64 lst else rid64 : lst
      addFolls  = L.nub . toggle . twitsFollow . snd . user
  runSqlPool (P.update keyid [TwitsFollow =. addFolls users, TwitsUpdated_at =. utc]) pool

-- TODO: Update MessageReply should take auth token and validate before action
whatUpdate :: ConnectionPool -> MessageReply -> IO ()
whatUpdate pool (MessageReply Nothing (Just _) Nothing (Just pid) rid) = updateMessageLikes pool pid rid
whatUpdate pool (MessageReply Nothing Nothing (Just _) (Just pid) rid) = do
  case pid == rid of
    True  -> pure ()
    False -> updateMessageFollows pool pid rid
whatUpdate _ (MessageReply _ _ _ Nothing _ ) = error "No pid should not happen at whatUpdate"
whatUpdate _ _ = undefined -- TODO: Update for reply messages

update :: ConnectionPool ->  Snap ()
update pool = do
  req <- getRequestBody
  case A.decode req of
    Just msgReply -> liftIO $ whatUpdate pool msgReply
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeLBS "{\"error\": \"Invalid JSON\"}"
