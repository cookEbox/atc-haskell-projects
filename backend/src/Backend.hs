{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import           Common.Route
import           Control.Monad.IO.Class  (liftIO)
-- import           Control.Monad.Logger    (runStdoutLoggingT)
import           Control.Monad.Logger    (runNoLoggingT)
import           Database.DB
import           Database.Persist.Sql    (runMigration)
import           Database.Persist.Sqlite (ConnectionPool, createSqlitePool,
                                          runSqlPool)
import           Obelisk.Backend
import           Obelisk.Route           as R
import           Routes.Login
import           Routes.Posted
import           Routes.Signup
import           Routes.Update
import           Routes.ProfileUp
import           Routes.Profile
import           Routes.WebSocket
import           Routes.Validate
import           Snap

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    pool <- runNoLoggingT $ createSqlitePool "Twits.db" 5
    runSqlPool (runMigration migrateAll) pool
    serve (backendHandlers pool)
  , _backend_routeEncoder = fullRouteEncoder
  }

backendHandlers :: ConnectionPool -> R BackendRoute -> Snap ()
backendHandlers pool = \case
  BackendRoute_Post      :/ () -> posted    pool
  BackendRoute_Login     :/ () -> login     pool
  BackendRoute_Logout    :/ () -> logout
  BackendRoute_Signup    :/ () -> signup    pool
  BackendRoute_Update    :/ () -> update    pool
  BackendRoute_Profile   :/ () -> profile   pool
  BackendRoute_ProfileUp :/ () -> profileUp pool
  BackendRoute_WebSocket :/ () -> websocket pool
  BackendRoute_Auth      :/ () -> auth 
  BackendRoute_Missing   :/ () -> do
    liftIO $ putStrLn "404: Route not found"
    modifyResponse $ setResponseStatus 404 "Not Found"
    writeBS "404 - Not Found"
