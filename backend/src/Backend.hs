{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import           Common.Route
import           Control.Monad.IO.Class  (liftIO)
import           Database.DB
import           Database.Persist.Sql    (runMigration)
import           Database.Persist.Sqlite (runSqlite)
import           Obelisk.Backend
import           Obelisk.Route           as R
import           Routes.Gotten
import           Routes.Login
import           Routes.Posted
import           Routes.Signup
import           Routes.Update
import           Snap

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    runSqlite "Twits.db" $ do runMigration migrateAll
    serve backendHandlers
  , _backend_routeEncoder = fullRouteEncoder
  }

backendHandlers :: R BackendRoute -> Snap ()
backendHandlers = \case
  BackendRoute_Post    :/ () -> posted
  BackendRoute_Get     :/ () -> gotten
  BackendRoute_Login   :/ () -> login
  BackendRoute_Logout  :/ () -> logout
  BackendRoute_Signup  :/ () -> signup
  BackendRoute_Update  :/ () -> update
  BackendRoute_Missing :/ () -> do
    liftIO $ putStrLn "404: Route not found"
    modifyResponse $ setResponseStatus 404 "Not Found"
    writeBS "404 - Not Found"
