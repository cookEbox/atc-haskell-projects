{-# LANGUAGE OverloadedStrings #-}

module Routes.Login where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import qualified Data.ByteString         as BS
import           Data.Time.Clock         (addUTCTime, getCurrentTime)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sqlite (runSqlite)
import           Shared.Functions
import           Crypto.KDF.BCrypt       (validatePassword)
import           Data.Text.Encoding      (encodeUtf8)
import           Snap

setCookie' :: (BS.ByteString, BS.ByteString) -> Snap () 
setCookie' = uncurry $ setCookie False

unSetCookie' :: BS.ByteString -> Snap () 
unSetCookie' name = setCookie False name ""

setCookie :: Bool -> BS.ByteString -> BS.ByteString -> Snap ()
setCookie expired name val = do
  now <- liftIO getCurrentTime
  let expires = case expired of 
                  False -> Just $ addUTCTime (60 * 60 * 24 * 7) now  -- 1 week
                  True -> Just now
      cookie = Cookie
        { cookieName     = name
        , cookieValue    = val
        , cookieExpires  = expires
        , cookieDomain   = Nothing
        , cookiePath     = Just "/"
        , cookieSecure   = False
        , cookieHttpOnly = False
        }
  modifyResponse $ addResponseCookie cookie

login :: Snap ()
login = do
  req <- getRequestBody
  case A.decode req of
    Just (UserDetailsReq username password) -> do
      maybeUser <- liftIO $ runSqlite "Twits.db" $ getBy (UniqueTwit username)
      case maybeUser of
        Just (Entity _ twit) ->
          if validatePassword (encodeUtf8 password) (encodeUtf8 $ twitsPassword twit)
            then do
              now <- liftIO getCurrentTime
              let token = AuthToken username now
              key <- liftIO getKey
              let signed = makeSignedToken key token
              modifyResponse $ setHeader "Content-Type" "application/json"
              setCookie' $ encodeUtf8 <$> ("auth", signed)
              setCookie' $ encodeUtf8 <$> ("user", username)
              setCookie' $ encodeUtf8 <$> ("status", "loggedIn")
              writeLBS (A.encode $ UserDetailsResp "Success")
            else do
              modifyResponse $ setResponseStatus 401 "Unauthorized"
              writeLBS "{\"error\": \"Invalid credentials\"}"
        Nothing -> do
          modifyResponse $ setResponseStatus 401 "Unauthorized"
          writeLBS "{\"error\": \"User not found\"}"
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      writeLBS "{\"error\": \"Invalid JSON\"}"

logout :: Snap ()
logout = do
  unSetCookie' $ encodeUtf8 "auth"
  unSetCookie' $ encodeUtf8 "user"
  unSetCookie' $ encodeUtf8 "status"
  writeLBS "{\"status\": \"Logged Out\"}"


