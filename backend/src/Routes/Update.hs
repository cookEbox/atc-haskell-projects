{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Update where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import           Shared.Functions
import           Snap

whatUpdate :: MessageReply -> IO ()
whatUpdate (MessageReply Nothing (Just _) Nothing (Just pid) rid) = updateMessageLikes pid rid
whatUpdate (MessageReply Nothing Nothing (Just _) (Just pid) rid) = do 
  case pid == rid of 
    True -> pure () 
    False -> updateMessageFollows pid rid
whatUpdate (MessageReply _ _ _ Nothing _ ) = error "No pid should not happen at whatUpdate"
whatUpdate _ = undefined -- TODO: Update for reply messages

update :: Snap ()
update = do
  req <- getRequestBody
  case A.decode req of
    Just msgReply -> liftIO $ whatUpdate msgReply
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeLBS "{\"error\": \"Invalid JSON\"}"
