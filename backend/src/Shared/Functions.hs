{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shared.Functions where

import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Crypto.Hash.Algorithms  (SHA256)
import           Crypto.KDF.BCrypt       (hashPassword)
import           Crypto.MAC.HMAC         hiding (update)
import           Data.Aeson              as A
import           Data.ByteArray          (convert)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.List               as L (delete, nub)
import           Data.Text               (Text, pack)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           Database.DB
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sql    (toSqlKey)
import           Database.Persist.Sqlite (runSqlite)
import           Maybes                  (isJust, rightToMaybe)
import           Prelude                 hiding (id)
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
  runSqlite "Twits.db" $ insert_ (Twits username hashed [])

updateMessageLikes :: Integer -> Integer -> IO ()
updateMessageLikes key rid = do
  eTweets <- liftIO $ runSqlite "Twits.db" $ selectList [] [Desc TweetsCreated_at]
  let tweets = (\(Entity id t) -> (id, t)) <$> eTweets
      keyid  = toSqlKey $ fromInteger key
      rid64  = fromInteger rid
      tweet  = head . filter (\id -> fst id == keyid)
      toggle lst = if elem rid64 lst then L.delete rid64 lst else rid64 : lst
      incLikes  = L.nub . toggle . tweetsLikes . snd . tweet
  runSqlite "Twits.db" $ update keyid [TweetsLikes =. incLikes tweets]

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

-- TODO: Remove this and source using a secure method
super_secret_DELETE :: IO String
super_secret_DELETE = pure "351c52add858652751a8dd19ad5a01c913d628abf41748021765428935e4ad11"

getKey :: IO BS.ByteString
getKey = fmap encodeUtf8 $ pack <$> super_secret_DELETE
-- getKey = fmap encodeUtf8 $ pack <$> getEnv "AUTH_SECRET"

validateAuthToken :: Text -> IO Bool
validateAuthToken token = do
  key <- getKey
  pure $ isJust (verifyToken key token)
