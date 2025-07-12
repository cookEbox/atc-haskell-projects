{-# LANGUAGE OverloadedStrings #-}

module Routes.Login where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Crypto.KDF.BCrypt       (validatePassword)
import           Data.Aeson              as A
import qualified Data.ByteString         as BS
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import           Data.Time.Clock         (NominalDiffTime, addUTCTime,
                                          getCurrentTime)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sql    (fromSqlKey)
import           Database.Persist.Sqlite (ConnectionPool, runSqlPool)
import           Prelude                 hiding (id)
import           Shared.Functions
import           Snap

oneWeek :: NominalDiffTime
oneWeek = 60 * 60 * 24 * 7

setCookie' :: (BS.ByteString, BS.ByteString) -> Snap ()
setCookie' = uncurry $ setCookie False

unSetCookie' :: BS.ByteString -> Snap ()
unSetCookie' name = setCookie False name ""

setCookie :: Bool -> BS.ByteString -> BS.ByteString -> Snap ()
setCookie expired name val = do
  now <- liftIO getCurrentTime
  let expires =
        case expired of
          False -> Just $ addUTCTime oneWeek now
          True  -> Just now
      cookie = Cookie
        { cookieName     = name
        , cookieValue    = val
        , cookieExpires  = expires
        , cookieDomain   = Nothing
        , cookiePath     = Just "/"
        , cookieSecure   = True
        , cookieHttpOnly = True
        }
  modifyResponse $ addResponseCookie cookie

ifMaybeUser :: Text
            -> Text
            -> Maybe (Entity Twits)
            -> Snap ()
ifMaybeUser username password (Just (Entity id twit)) =
  if validatePassword (encodeUtf8 password) (encodeUtf8 $ twitsPassword twit)
  then do
    now <- liftIO getCurrentTime
    key <- liftIO getKey
    let id_    = toInteger $ fromSqlKey id
        token  = AuthToken username id_ now
        signed = makeSignedToken key token

    setCookie' $ encodeUtf8 <$> ("session", signed)
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS (A.encode $ UserDetailsResp "Success")
  else do
    modifyResponse $ setResponseStatus 401 "Unauthorized"
    writeLBS . A.encode $ A.object ["error" .= ("Invalid credentials" :: Text)]
ifMaybeUser _ _ Nothing = do
  modifyResponse $ setResponseStatus 401 "Unauthorized"
  writeLBS . A.encode $ A.object ["error" .= ("User not found" :: Text)]

login :: ConnectionPool -> Snap ()
login pool = do
  req <- getRequestBody
  case A.decode req of
    Just (UserDetailsReq username password) -> do
      maybeUser <- liftIO $ runSqlPool (getBy (UniqueTwit username)) pool
      ifMaybeUser username password maybeUser
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      writeLBS . A.encode $ A.object ["error" .= ("Invalid JSON" :: Text)]

logout :: Snap ()
logout = do
  unSetCookie' $ encodeUtf8 "session"
  writeLBS . A.encode $ A.object ["status" .= ("Logged Out" :: Text)]


