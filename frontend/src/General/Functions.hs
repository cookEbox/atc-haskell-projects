{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module General.Functions where

import           Common.Api
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (ToJSON)
import           Data.Map.Strict             (singleton, (!))
import           Data.Maybe                  (listToMaybe)
import           Data.Text                   (Text, isInfixOf, splitOn, strip,
                                              stripPrefix, unpack)
import           Data.Time.Clock             (getCurrentTime)
import           Language.Javascript.JSaddle (JSM, MonadJSM, eval, liftJSM,
                                              strToText, valToStr)
import           Reflex.Dom.Core

getCookies :: JSM Text
getCookies = strToText <$> (valToStr =<< eval ("document.cookie" :: Text))

cookieGetter :: (MonadWidget t m) => m (Dynamic t Text)
cookieGetter = do
  getter <- getPostBuild
  cookieEvent <- performEvent (liftJSM getCookies <$ getter)
  holdDyn "" cookieEvent

cookieWatcher :: (MonadWidget t m) => m (Dynamic t Text)
cookieWatcher = do
  tick <- tickLossy 0.1 =<< liftIO getCurrentTime
  cookieEvent <- performEvent (liftJSM getCookies <$ tick)
  holdDyn "" cookieEvent

statusCookieMaybe :: Text -> Maybe Text
statusCookieMaybe cookieDyn =
  case isInfixOf "status=loggedIn" cookieDyn of
    True  -> Just cookieDyn
    False -> Nothing

data User = User { username  :: Text    } deriving stock Eq
data Auth = Auth { authtoken :: Text    } deriving stock Eq
data UID  = UID  { userid    :: Integer } deriving stock Eq
type CookieData = Maybe (Auth, User, UID)

parseCookie :: Text -> CookieData
parseCookie cookieText =
  let cookies = map strip $ splitOn ";" cookieText
      authVal = Auth <$> listToMaybe [val | entry <- cookies, Just val <- [stripPrefix "auth=" entry]]
      userVal = User <$> listToMaybe [val | entry <- cookies, Just val <- [stripPrefix "user=" entry]]
      idVal   = UID . read . unpack  <$> listToMaybe [val | entry <- cookies, Just val <- [stripPrefix "id=" entry]]
  in (,,) <$> authVal <*> userVal <*> idVal

data AppState t = AppState
  { appLoggedIn    :: Dynamic t (Maybe UserInfo)
  , refreshUserReq :: () -> IO ()
  , profileUidDyn  :: Dynamic t (Maybe Integer)
  , profileClick   :: Integer -> IO ()
  } 

flattenDyn :: Reflex t => Dynamic t (Dynamic t a) -> Dynamic t a
flattenDyn dd =
  (\mp -> mp ! ()) <$> joinDynThroughMap (singleton () <$> dd)

sendRequest :: ( MonadJSM
               ( Performable m )
               , PerformEvent t m
               , TriggerEvent t m
               , ToJSON a)
            => Text -> Event t a -> m (Event t XhrResponse)
sendRequest path event = performRequestAsync $ fmap (postJson ("http://localhost:8000/" <> path)) event

tagger :: Reflex t
       => Dynamic t Text
       -> Dynamic t Text
       -> Event t a
       -> Event t UserDetailsReq
tagger usernameDyn hashedPasswordDyn loginEv =
  tag (current $ UserDetailsReq <$> usernameDyn <*> hashedPasswordDyn) loginEv

updateState :: (PerformEvent t1 m, MonadJSM (Performable m))
            => AppState t2 -> Event t1 a -> m ()
updateState appState success = do
    performEvent_ $ ffor success $ \_ -> liftJSM $ do
      liftIO $ refreshUserReq appState ()

