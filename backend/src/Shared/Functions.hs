{-# LANGUAGE OverloadedStrings #-}

module Shared.Functions where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import           Data.ByteArray          (convert)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Char8   as BS8
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text               (Text, pack)
import           Data.Time.Clock         (addUTCTime, getCurrentTime)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sqlite (runSqlite)
import           Maybes                  (rightToMaybe)
import           Crypto.Hash.Algorithms  (SHA256)
import           Crypto.KDF.BCrypt       (hashPassword)
import           Crypto.MAC.HMAC
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           Snap
-- import           System.Environment      (getEnv)
import qualified System.IO.Streams       as Streams (toList)

getRequestBody :: MonadSnap m => m LBS.ByteString
getRequestBody = LBS.fromChunks <$> runRequestBody Streams.toList

hashPasswordSecure :: Text -> IO Text
hashPasswordSecure password = do
  hashed <- hashPassword 12 (encodeUtf8 password)
  return $ decodeUtf8 hashed

storeUser :: Text -> Text -> IO ()
storeUser username password = do
  hashed <- hashPasswordSecure password
  runSqlite "Twits.db" $ insert_ (Twits username hashed)

signToken :: BS.ByteString -> AuthToken -> BS.ByteString
signToken key token =
  BS.append payload sig
  where
    payload = LBS.toStrict $ A.encode token
    hmacDigest = hmac key payload :: HMAC SHA256
    sig = convert (hmacGetDigest hmacDigest)

makeSignedToken :: BS.ByteString -> AuthToken -> Text
makeSignedToken key token = decodeUtf8 . B64.encode $ signToken key token

verifyToken :: BS.ByteString -> Text -> Maybe AuthToken
verifyToken key encoded = do
  raw <- rightToMaybe $ B64.decode (encodeUtf8 encoded)
  let (payload, sig) = BS.splitAt (BS.length raw - 32) raw
      expected = convert $ hmacGetDigest (hmac key payload :: HMAC SHA256)
  if sig == expected
  then decodeStrict payload
  else Nothing

authCookieName :: BS.ByteString
authCookieName = "auth"

super_secret_DELETE :: IO String
super_secret_DELETE = pure "351c52add858652751a8dd19ad5a01c913d628abf41748021765428935e4ad11"

getKey :: IO BS.ByteString
getKey = fmap encodeUtf8 $ pack <$> super_secret_DELETE
-- getKey = fmap encodeUtf8 $ pack <$> getEnv "AUTH_SECRET"

setAuthCookie :: BS.ByteString -> Snap ()
setAuthCookie val = do
  now <- liftIO getCurrentTime
  let expires = Just $ addUTCTime (60 * 60 * 24 * 7) now  -- 1 week
      cookie = Cookie
        { cookieName     = "auth"
        , cookieValue    = val
        , cookieExpires  = expires
        , cookieDomain   = Nothing
        , cookiePath     = Just "/"
        , cookieSecure   = False
        , cookieHttpOnly = True       -- inaccessible to JS
        }
  modifyResponse $ addResponseCookie cookie

handleAuthCheck :: Snap ()
handleAuthCheck = do
  mCookie <- getCookie "auth"
  case mCookie of
    Nothing -> writeLBS "No token"
    Just c -> do
      key <- liftIO (BS8.pack <$> super_secret_DELETE )
      -- key <- liftIO (BS8.pack <$> getEnv "AUTH_SECRET")
      let encoded = cookieValue c
      case B64.decode encoded of
        Left _ -> writeBS "Invalid base64"
        Right raw ->
          case verifyToken key (pack . BS8.unpack $ raw) of
            Nothing  -> writeBS "Invalid signature"
            Just tok -> writeBS $ encodeUtf8 (authUserId tok)

