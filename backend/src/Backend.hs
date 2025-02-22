{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}

module Backend where

import           Common.Route
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text               (Text, pack)
import           Data.Time.Clock         (UTCTime, getCurrentTime)
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sql    (runMigration)
import           Database.Persist.Sqlite (runSqlite)
import           Database.Persist.TH

import           GHC.Int                 (Int64)
import           Obelisk.Backend
import           Obelisk.Route           as R
import           Snap
import qualified System.IO.Streams       as Streams (toList)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Twats
    name Text
    deriving Show
Twits
    user_id        Int64
    parent_post_id (Maybe Int64)
    content        Text
    created_at     UTCTime
    deriving Show Eq
|]

getRequestBody :: MonadSnap m => m LBS.ByteString
getRequestBody = LBS.fromChunks <$> runRequestBody Streams.toList

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    runSqlite "Twits.db" $ do runMigration migrateAll
    serve backendHandlers
  , _backend_routeEncoder = fullRouteEncoder
  }

backendHandlers :: R BackendRoute -> Snap ()
backendHandlers = \case
  BackendRoute_Post :/ () -> do
    req <- getRequestBody
    case A.decode req of
      Just (MessageReq input) -> do
        utc <- liftIO getCurrentTime
        let newTwit = Twits 1 Nothing input utc
        twitId <- liftIO $ runSqlite "Twits.db" $ insert newTwit
        let response = MessageResp $ ("Your input was: " <> input <> "\nYour Id is: " <> (pack . show $ twitId)) : []
        modifyResponse $ setHeader "Content-Type" "application/json"
        writeLBS (A.encode response)  -- Send JSON response to frontend

      Nothing -> do
        modifyResponse $ setResponseStatus 400 "Bad Request"
        modifyResponse $ setHeader "Content-Type" "application/json"
        writeLBS "{\"error\": \"Invalid JSON\"}"  -- Send error response

  BackendRoute_Get :/ () -> do
    (eTwits) <- liftIO $ runSqlite "Twits.db" $ selectList [] [Desc TwitsCreated_at]
    let twits = (\(Entity _ t) -> t) <$> eTwits
        response = MessageResp $ fmap (twitsContent) twits
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS (A.encode response)  -- Send JSON response to frontend


  BackendRoute_Missing :/ () -> do
    liftIO $ putStrLn "404: Route not found"
    modifyResponse $ setResponseStatus 404 "Not Found"
    writeBS "404 - Not Found"
