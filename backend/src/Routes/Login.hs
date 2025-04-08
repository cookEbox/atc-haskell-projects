{-# LANGUAGE OverloadedStrings #-}

module Routes.Login where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import           Data.Time.Clock         (getCurrentTime)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sqlite (runSqlite)
import           Shared.Functions
import           Crypto.KDF.BCrypt       (validatePassword)
import           Data.Text.Encoding      (encodeUtf8)
import           Snap

login :: Snap ()
login = do
  req <- getRequestBody
  case A.decode req of
    Just (LoginReq username password) -> do
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
              setAuthCookie (encodeUtf8 signed)
              writeLBS (A.encode $ LoginResp "Success")
            else do
              modifyResponse $ setResponseStatus 401 "Unauthorized"
              writeLBS "{\"error\": \"Invalid credentials\"}"
        Nothing -> do
          modifyResponse $ setResponseStatus 401 "Unauthorized"
          writeLBS "{\"error\": \"User not found\"}"
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      writeLBS "{\"error\": \"Invalid JSON\"}"

handleLogout :: Snap ()
handleLogout = do
  now <- liftIO getCurrentTime
  let expiredCookie = Cookie
                      { cookieName     = "auth"
                      , cookieValue    = ""
                      , cookieExpires  = Just now
                      , cookieDomain   = Nothing
                      , cookiePath     = Just "/"
                      , cookieSecure   = False
                      , cookieHttpOnly = True
                      }
  modifyResponse $ addResponseCookie expiredCookie
  writeLBS "{\"status\": \"Logged Out\"}"


