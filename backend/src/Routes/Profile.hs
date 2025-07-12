{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Profile where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A hiding (Key)
import           Database.DB
import           Database.Persist        as P hiding (Add, count)
import           Database.Persist.Sql    (fromSqlKey)
import           Database.Persist.Sqlite (ConnectionPool, runSqlPool)
import           Prelude                 hiding (id)
import           Routes.Validate
import           Shared.Functions
import           Snap

createUserProfile :: Maybe Twits -> Key Twits -> UserProfile
createUserProfile Nothing _ =
  UserProfile Nothing Nothing Nothing Nothing Nothing Nothing
createUserProfile (Just user) twitKey =
  UserProfile (Just $ twitsName user)
              (twitsDob user)
              (twitsLocation user)
              (twitsHobbies user)
              (twitsBio user)
              (Just $ toInteger $ fromSqlKey twitKey)

getProfile :: ConnectionPool -> Integer -> Snap UserProfile
getProfile pool uid = do
  let twitKey :: TwitsId = intToSqlKey uid
  userMb <- liftIO $ runSqlPool (P.get twitKey) pool
  pure $ createUserProfile userMb twitKey

profile :: ConnectionPool ->  Snap ()
profile pool = do
  req <- getRequestBody
  authorised <- validate
  case authorised of
    Just _ ->
      case A.decode req of
        Just piid -> do
          userProfile <- getProfile pool (piId piid)
          modifyResponse $ setHeader "Content-Type" "application/json"
          writeLBS (A.encode userProfile)
        Nothing -> do
          modifyResponse $ setResponseStatus 400 "Bad Request"
          modifyResponse $ setHeader "Content-Type" "application/json"
          writeAesonObject "error" "Invalid JSON"
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeAesonObject "error" "Not logged in"
