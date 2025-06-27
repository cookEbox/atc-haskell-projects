{-# LANGUAGE OverloadedStrings #-}

module Routes.Login where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Crypto.KDF.BCrypt       (validatePassword)
import           Data.Aeson              as A
import qualified Data.ByteString         as BS
import           Data.Text               (Text, pack)
import           Data.Text.Encoding      (encodeUtf8)
import           Data.Time.Clock         (addUTCTime, getCurrentTime)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sql    (fromSqlKey)
import           Database.Persist.Sqlite (runSqlPool, ConnectionPool)
import           Prelude                 hiding (id)
import           Shared.Functions
import           Snap

setCookie' :: (BS.ByteString, BS.ByteString) -> Snap ()
setCookie' = uncurry $ setCookie False

unSetCookie' :: BS.ByteString -> Snap ()
unSetCookie' name = setCookie False name ""

setCookie :: Bool -> BS.ByteString -> BS.ByteString -> Snap ()
setCookie expired name val = do
  now <- liftIO getCurrentTime
  let expires = 
        case expired of
          False -> Just $ addUTCTime (60 * 60 * 24 * 7) now  -- 1 week
          True  -> Just now
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

cookiesSet :: Text -> Text -> Text -> Snap ()
cookiesSet signed username id_ = do
  setCookie' $ encodeUtf8 <$> ("session", signed     )
  setCookie' $ encodeUtf8 <$> ("user"   , username   )
  setCookie' $ encodeUtf8 <$> ("id"     , id_        )
  setCookie' $ encodeUtf8 <$> ("status" , "loggedIn" )

ifMaybeUser :: Text
            -> Text
            -> Maybe (Entity Twits)
            -> Snap ()
ifMaybeUser username password (Just (Entity id twit)) =
  if validatePassword (encodeUtf8 password) (encodeUtf8 $ twitsPassword twit)
  then do
    now <- liftIO getCurrentTime
    key <- liftIO getKey
    let token = AuthToken username now
        signed = makeSignedToken key token
        id_ = pack . show $ fromSqlKey id
    cookiesSet signed username id_
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS (A.encode $ UserDetailsResp "Success")
  else do
    modifyResponse $ setResponseStatus 401 "Unauthorized"
    writeLBS "{\"error\": \"Invalid credentials\"}"
ifMaybeUser _ _ Nothing = do
  modifyResponse $ setResponseStatus 401 "Unauthorized"
  writeLBS "{\"error\": \"User not found\"}"

login :: ConnectionPool -> Snap ()
login pool = do
  req <- getRequestBody
  case A.decode req of
    Just (UserDetailsReq username password) -> do
      maybeUser <- liftIO $ runSqlPool (getBy (UniqueTwit username)) pool
      ifMaybeUser username password maybeUser
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      writeLBS "{\"error\": \"Invalid JSON\"}"

logout :: Snap ()
logout = do
  unSetCookie' $ encodeUtf8 "auth"
  unSetCookie' $ encodeUtf8 "user"
  unSetCookie' $ encodeUtf8 "status"
  writeLBS "{\"status\": \"Logged Out\"}"


