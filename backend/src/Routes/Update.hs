{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Routes.Update where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A hiding (Key)
import qualified Data.List               as L (delete, nub)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time.Clock         (getCurrentTime)
import           Database.DB
import           Database.Persist        as P hiding (Add, count)
import           Database.Persist.Sql    (toSqlKey, SqlBackend)
import           Database.Persist.Sqlite (ConnectionPool, runSqlPool)
import           GHC.Int                 (Int64)
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
  runSqlPool ( P.update keyid [ TweetsLikes      =. incLikes tweets
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
      (pidKey, pid64) = keyAnd64 pid
      (ridKey, rid64) = keyAnd64 rid
      follower   = head . filter (\id -> fst id == pidKey)
      following  = head . filter (\id -> fst id == ridKey)
      toggle rok lst = if   elem rok lst
                       then L.delete rok lst
                       else rok : lst
      addFollowers rok = L.nub . toggle rok . twitsFollowers . snd . follower
      addFollowing rok = L.nub . toggle rok . twitsFollowing . snd . following
  runSqlPool ( P.update pidKey [ TwitsFollowers  =. addFollowers rid64 users
                               , TwitsUpdated_at =. utc
                               ]
             ) pool
  runSqlPool ( P.update ridKey [ TwitsFollowing  =. addFollowing pid64 users
                               , TwitsUpdated_at =. utc
                               ]
             ) pool

-- TODO: Update MessageReply should take auth token and validate before action
whatUpdate :: ConnectionPool -> MessageReply -> IO Text
whatUpdate pool (MessageReply Nothing Like (Just pid) rid (Just authToken)) 
  = do authorised <- liftIO $ validateAuthToken authToken
       case authorised of 
         True -> do updateMessageLikes pool pid rid
                    pure ""
         False -> pure "Unauthorised"
whatUpdate pool (MessageReply Nothing Follow (Just pid) rid (Just authToken)) 
  = do authorised <- liftIO $ validateAuthToken authToken 
       case authorised of
         True  -> if pid /= rid 
                  then do updateMessageFollows pool pid rid 
                          pure ""
                  else pure ""
         False -> pure ""
whatUpdate _ (MessageReply _ _ Nothing _ _) 
  = error "No pid should not happen at whatUpdate"
whatUpdate _ (MessageReply _ _ _ _ Nothing) 
  = pure "Authorise Token required"
whatUpdate _ _ = undefined -- TODO: Update for reply messages

update :: ConnectionPool ->  Snap ()
update pool = do
  req <- getRequestBody
  case A.decode req of
    Just msgReply -> do 
      err <- liftIO $ whatUpdate pool msgReply
      if not (T.null err) 
      then writeLBS (encode $ object ["error" .= err])
      else pure ()
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeLBS (encode $ object ["error" .= ("Invalid JSON" :: T.Text)])

