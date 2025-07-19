{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shared.Functions where

import           Common.Api
import           Crypto.Hash.Algorithms  (SHA256)
import           Crypto.KDF.BCrypt       (hashPassword)
import           Crypto.MAC.HMAC         hiding (update)
import           Data.Aeson              ((.=))
import qualified Data.Aeson              as A
import           Data.ByteArray          (convert)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Char8   as B8
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text               (Text)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           Data.Time.Clock         (getCurrentTime)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sqlite (runSqlite, toSqlKey)
import           Maybes                  (rightToMaybe)
import           Prelude                 hiding (id)
import           Snap
import qualified System.IO.Streams       as Streams (toList)
import           System.Environment      (lookupEnv)

writeAesonObject :: MonadSnap m => A.Key -> Text -> m ()
writeAesonObject ky vlu = do
  writeLBS . A.encode $ A.object [ky .= vlu]

intToSqlKey :: Integer -> Key Twits
intToSqlKey = toSqlKey . fromIntegral

getRequestBody :: MonadSnap m => m LBS.ByteString
getRequestBody = LBS.fromChunks <$> runRequestBody Streams.toList

hashPasswordSecure :: Text -> IO Text
hashPasswordSecure password = do
  hashed <- hashPassword 12 (encodeUtf8 password)
  return $ decodeUtf8 hashed

storeUser :: Text -> Text -> IO ()
storeUser username password = do
  utc <- getCurrentTime
  hashed <- hashPasswordSecure password
  runSqlite "Twits.db" $ insert_ 
    ( Twits username 
            hashed 
            [] 
            [] 
            utc 
            Nothing 
            Nothing 
            Nothing 
            Nothing 
    )

signToken :: BS.ByteString -> AuthToken -> BS.ByteString
signToken key token =
  BS.append payload sig
  where
    payload = LBS.toStrict $ A.encode token
    hmacDigest = hmac key payload :: HMAC SHA256
    sig = convert (hmacGetDigest hmacDigest)

makeSignedToken :: BS.ByteString -> AuthToken -> Text
makeSignedToken key token = decodeUtf8 . B64.encode $ signToken key token

verifyToken :: BS.ByteString -> BS.ByteString -> Maybe AuthToken
verifyToken key encoded = do
  raw <- rightToMaybe $ B64.decode encoded
  let (payload, sig) = BS.splitAt (BS.length raw - 32) raw
      expected = convert $ hmacGetDigest (hmac key payload :: HMAC SHA256)
  if sig == expected
  then A.decodeStrict payload
  else Nothing

authCookieName :: BS.ByteString
authCookieName = "auth"

getKey :: IO B8.ByteString
getKey = do
  mEnv <- lookupEnv "AUTH_SECRET"
  case mEnv of
    Just s  -> pure (B8.pack s)
    Nothing -> B8.readFile "config/auth-secret.txt"
