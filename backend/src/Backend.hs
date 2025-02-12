{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import           Common.Route
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import qualified Data.ByteString.Lazy    as LBS
import           Obelisk.Backend
import           Obelisk.Route           as R
import           Snap
import qualified System.IO.Streams       as Streams (toList)

getRequestBody :: MonadSnap m => m LBS.ByteString
getRequestBody = LBS.fromChunks <$> runRequestBody Streams.toList

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
    BackendRoute_Echo :/ () -> do
      req <- getRequestBody
      -- liftIO $ print "HELLO THERE AM I COMING THROUGHTASDLKJFA:SLDKFAS:LDFJA:SDLFKJAS:DLKFJ"
      -- liftIO $ print req
        -- Decode the request
      case A.decode req of
        Just (MessageReq input) -> do
          let response = MessageResp ("Your input was: " <> input)
          modifyResponse $ setHeader "Content-Type" "application/json"
          writeLBS (A.encode response)  -- Send JSON response to frontend

        Nothing -> do
          modifyResponse $ setResponseStatus 400 "Bad Request"
          modifyResponse $ setHeader "Content-Type" "application/json"
          writeLBS "{\"error\": \"Invalid JSON\"}"  -- Send error response

    BackendRoute_Missing :/ () -> do
      liftIO $ putStrLn "404: Route not found"
      modifyResponse $ setResponseStatus 404 "Not Found"
      writeBS "404 - Not Found"
  , _backend_routeEncoder = fullRouteEncoder
  }
