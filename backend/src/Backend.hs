{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}

module Backend where

import           Common.Route
import           Common.Api
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              as A
import           Data.ByteArray          (convert)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString.Char8   as BS8
import           Data.Text               (Text, pack)
import           Data.Time.Clock         (UTCTime, getCurrentTime, addUTCTime)
import           Database.Persist        hiding (Add, count)
import           Database.Persist.Sql    (runMigration)
import           Database.Persist.Sqlite (runSqlite)
import           Database.Persist.TH
import           Maybes                  (rightToMaybe)

import           Crypto.Hash.Algorithms  (SHA256)
import           Crypto.KDF.BCrypt       (hashPassword, validatePassword)
import           Crypto.MAC.HMAC
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           GHC.Int                 (Int64)
import           Obelisk.Backend
import           Obelisk.Route           as R
import           Snap
-- import           System.Environment      (getEnv)
import qualified System.IO.Streams       as Streams (toList)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Twits
    name Text
    password Text
    UniqueTwit name
    deriving Show
Tweets
    user_id        Int64
    user_name      Text
    parent_post_id (Maybe Int64)
    content        Text
    created_at     UTCTime
    deriving Show Eq
|]

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

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    runSqlite "Twits.db" $ do runMigration migrateAll
    serve backendHandlers
  , _backend_routeEncoder = fullRouteEncoder
  }

posted :: Snap ()
posted = do
  req <- getRequestBody
  case A.decode req of
    Just (MessageReq user reqMsg) -> do
      utc <- liftIO getCurrentTime
      let newTweet = Tweets 1 user Nothing reqMsg utc
      tweetId <- liftIO $ runSqlite "Twits.db" $ insert newTweet
      let response = MessageResp
                      { responseMsg = [
                                        ( user, ( "Your input was: "
                                                <> reqMsg
                                                <> "\nYour Id is: "
                                                <> (pack . show $ tweetId)
                                                )
                                        )
                                      ]
                      }
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeLBS (A.encode response)  -- Send JSON response to frontend

    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeLBS "{\"error\": \"Invalid JSON\"}"  -- Send error response

gotten :: Snap ()
gotten = do
  (eTweets) <- liftIO $ runSqlite "Twits.db" $ selectList [] [Desc TweetsCreated_at]
  let tweets = (\(Entity _ t) -> t) <$> eTweets
      response = MessageResp
                  { responseMsg = (\t -> (tweetsUser_name t, tweetsContent t)) <$> tweets
                  }
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS (A.encode response)  -- Send JSON response to frontend

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
            Nothing -> writeBS "Invalid signature"
            Just tok -> writeBS $ encodeUtf8 (authUserId tok)

signup :: Snap ()
signup = do
  req <- getRequestBody
  case A.decode req of
    Just (LoginReq username password) -> do
      maybeUser <- liftIO $ runSqlite "Twits.db" $ getBy (UniqueTwit username)
      case maybeUser of
        Just (Entity _ _) -> do
          modifyResponse $ setResponseStatus 401 "Unauthorized"
          writeLBS "{\"error\": \"User already exists\"}"
        Nothing -> do
          liftIO $ storeUser username password
    Nothing -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      writeLBS "{\"error\": \"Invalid JSON\"}"

backendHandlers :: R BackendRoute -> Snap ()
backendHandlers = \case
  BackendRoute_Post :/ () -> posted
  BackendRoute_Get :/ () -> gotten
  BackendRoute_Login :/ () -> login
  BackendRoute_Signup :/ () -> signup
  BackendRoute_Missing :/ () -> do
    liftIO $ putStrLn "404: Route not found"
    modifyResponse $ setResponseStatus 404 "Not Found"
    writeBS "404 - Not Found"
