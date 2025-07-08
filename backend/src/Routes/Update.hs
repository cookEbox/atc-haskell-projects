{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Routes.Update where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A hiding (Key)
import qualified Data.List               as L (delete, nub)
import           Data.Time.Clock         (getCurrentTime)
import           Database.DB
import           Database.Persist        as P hiding (Add, count)
import           Database.Persist.Sql    (toSqlKey, SqlBackend)
import           Database.Persist.Sqlite (ConnectionPool, runSqlPool)
import           GHC.Int                 (Int64)
import           Prelude                 hiding (id)
import           Shared.Functions
import Routes.Validate
import           Snap

updateMessageLikes :: ConnectionPool -> Integer -> Integer -> IO ()
updateMessageLikes pool pid rid = do
  eTweets <- liftIO $ runSqlPool (selectList [] [Desc TweetsCreated_at]) pool
  utc <- liftIO getCurrentTime
  let tweets = (\(Entity id t) -> (id, t)) <$> eTweets
      pidKey  = toSqlKey $ fromInteger pid
      ridKey  = toSqlKey $ fromInteger rid
      tweet  = head . filter (\id -> fst id == pidKey)
      toggle lst = if elem ridKey lst then L.delete ridKey lst else ridKey : lst
      incLikes  = L.nub . toggle . tweetsLikes . snd . tweet
  runSqlPool ( P.update pidKey [ TweetsLikes      =. incLikes tweets
                              , TweetsUpdated_at =. utc
                              ]
             ) pool

keyAnd64 :: forall record. ToBackendKey SqlBackend record 
         => Integer -> (Key record, Int64)
keyAnd64 n = (toSqlKey n64, n64)
  where
    n64 = fromInteger n

updateMessageFollows :: ConnectionPool -> Integer -> Integer -> IO ()
updateMessageFollows pool pid rid = do
  eUsers <- liftIO $ runSqlPool (selectList [] [Desc TwitsName]) pool
  utc <- liftIO getCurrentTime
  let users  = (\(Entity id u) -> (id, u)) <$> eUsers
      pidKey = intToSqlKey pid
      ridKey = intToSqlKey rid
      follower   = head . filter (\id -> fst id == pidKey)
      following  = head . filter (\id -> fst id == ridKey)
      toggle rok lst = if   elem rok lst
                       then L.delete rok lst
                       else rok : lst
      addFollowers rok = L.nub . toggle rok . twitsFollowers . snd . follower
      addFollowing rok = L.nub . toggle rok . twitsFollowing . snd . following
  runSqlPool ( P.update pidKey [ TwitsFollowers  =. addFollowers ridKey users
                               , TwitsUpdated_at =. utc
                               ]
             ) pool
  runSqlPool ( P.update ridKey [ TwitsFollowing  =. addFollowing pidKey users
                               , TwitsUpdated_at =. utc
                               ]
             ) pool

whatUpdate :: ConnectionPool -> MessageReply -> UserInfo -> IO ()
whatUpdate pool (MessageReply Nothing Like (Just pid) rid) userInfo
  = if rid == uiId userInfo
    then updateMessageLikes pool pid rid
    else pure ()
whatUpdate pool (MessageReply Nothing Follow (Just pid) rid) userInfo 
  = do
      if rid == uiId userInfo
      then case pid == rid of
            True  -> pure ()
            False -> updateMessageFollows pool pid rid
      else pure ()
whatUpdate _ (MessageReply _ _ Nothing _ ) _
  = error "No pid should not happen at whatUpdate"
whatUpdate _ _ _= undefined -- TODO: Update for reply messages

update :: ConnectionPool ->  Snap ()
update pool = do
  req <- getRequestBody
  authorised <- validate
  case authorised of 
    Just userInfo ->
      case A.decode req of
        Just msgReply -> liftIO $ whatUpdate pool msgReply userInfo
        Nothing -> do
          modifyResponse $ setResponseStatus 400 "Bad Request"
          modifyResponse $ setHeader "Content-Type" "application/json"
          writeLBS "{\"error\": \"Invalid JSON\"}"
    Nothing -> do
          modifyResponse $ setResponseStatus 400 "Bad Request"
          modifyResponse $ setHeader "Content-Type" "application/json"
          writeLBS "{\"error\": \"Not logged in\"}"
