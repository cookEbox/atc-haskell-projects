{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.ProfileUp where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A hiding (Key)
import           Data.Maybe              (catMaybes)
import           Database.DB
import           Database.Persist        as P hiding (Add, count)
import           Database.Persist.Sql    (toSqlKey)
import           Database.Persist.Sqlite (ConnectionPool, runSqlPool)
import           Prelude                 hiding (id)
import           Routes.Validate
import           Shared.Functions
import           Snap

(.=?) :: PersistField a 
      => EntityField Twits a 
      -> Maybe a 
      -> Maybe (Update Twits)
(.=?) field mval = fmap (field =.) mval
infixr 2 .=?

(.?=) :: PersistField a 
      => EntityField Twits (Maybe a) 
      -> Maybe a 
      -> Maybe (Update Twits)
field .?= mval = fmap (\x -> field =. Just x) mval
infixr 2 .?=

-- TODO: Update updated_at so changes are picked up for name
updateUserFromProfile :: ConnectionPool -> TwitsId -> UserProfile -> IO ()
updateUserFromProfile pool userId UserProfile{..} = do
  let updates :: [Update Twits]
      updates = catMaybes
        [ TwitsName      .=? prName
        , TwitsDob       .?= prDOB
        , TwitsLocation  .?= prLocation
        , TwitsHobbies   .?= prHobbies
        , TwitsBio       .?= prBio
        ]
  runSqlPool (P.update userId updates) pool

updateProfile :: ConnectionPool -> UserProfile -> UserInfo -> Snap ()
updateProfile pool userProfile (UserInfo _ uid) =
  case equalMb $ prId userProfile of
    True  -> liftIO $ updateUserFromProfile pool (toSqlKey $ fromInteger uid) userProfile
    False -> pure ()
  where
    equalMb Nothing    = False
    equalMb (Just pid) = uid == pid

profileUp :: ConnectionPool ->  Snap ()
profileUp pool = do
  req <- getRequestBody
  authorised <- validate
  case authorised of
    Just auid ->
      case A.decode req of
        Just upid -> do updateProfile pool upid auid
                        modifyResponse $ setHeader "Content-Type" "application/json"
                        writeLBS "{\"Success\": \"Updated Profile\"}"
        Nothing -> do
          modifyResponse $ setResponseStatus 400 "Bad Request"
          modifyResponse $ setHeader "Content-Type" "application/json"
          writeLBS "{\"error\": \"Invalid JSON\"}"
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeLBS "{\"error\": \"Not logged in\"}"
