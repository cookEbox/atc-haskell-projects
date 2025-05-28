{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module General.Functions where

import           Common.Route
import           Control.Monad               ((>=>))
import           Control.Monad.IO.Class      (liftIO)
import Data.Aeson (ToJSON)
import           Data.Map.Strict             (singleton, (!))
import           Data.Maybe                  (listToMaybe)
import           Data.Text                   (Text, isInfixOf, splitOn, strip,
                                              stripPrefix)
import           Data.Time.Clock             (getCurrentTime)
import           Language.Javascript.JSaddle (JSM, eval, liftJSM, strToText,
                                              valToStr, MonadJSM)
import           Obelisk.Frontend
import           Obelisk.Route
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
  tick <- tickLossy 1 =<< liftIO getCurrentTime
  cookieEvent <- performEvent (liftJSM getCookies <$ tick)
  holdDyn "" cookieEvent

statusCookie :: Functor f => f Text -> f Bool
statusCookie cookieDyn = isInfixOf "status=loggedIn" <$> cookieDyn

statusCookieMaybe :: Text -> Maybe Text
statusCookieMaybe cookieDyn =
  case isInfixOf "status=loggedIn" cookieDyn of
    True  -> Just cookieDyn
    False -> Nothing

parseCookie :: Text -> Maybe (Text, Text)
parseCookie cookieText =
  let cookies = map strip $ splitOn ";" cookieText
      authVal = listToMaybe [val | entry <- cookies, Just val <- [stripPrefix "auth=" entry]]
      userVal = listToMaybe [val | entry <- cookies, Just val <- [stripPrefix "user=" entry]]
  in (,) <$> authVal <*> userVal

data AppState t = AppState
  { appLoggedIn     :: Dynamic t (Maybe (Text, Text))
  , triggerLoggedIn :: Maybe (Text, Text) -> IO ()
  }

flattenDyn :: Reflex t => Dynamic t (Dynamic t a) -> Dynamic t a
flattenDyn dd =
  (\mp -> mp ! ()) <$> joinDynThroughMap (singleton () <$> dd)

initial :: ObeliskWidget t (R FrontendRoute) m => m (Dynamic t (Maybe (Text, Text)))
initial = do
  nestedDyn <- prerender
    (pure $ constDyn Nothing)
    (do
      cookieDyn       <- cookieWatcher
      let parsedDyn    = fmap (statusCookieMaybe >=> parseCookie) cookieDyn
      firstParsedE    <- headE $ fmapMaybe id (updated parsedDyn)
      oneAndDoneDyn   <- holdDyn Nothing (Just <$> firstParsedE)
      pure oneAndDoneDyn
    )
  pure $ flattenDyn nestedDyn

buildAppState :: forall t m. ObeliskWidget t (R FrontendRoute) m => m (AppState t)
buildAppState = do
  initialLoggedIn <- initial
  (loginEvent, triggerLogin) <- newTriggerEvent
  loginStateDyn <- holdDyn Nothing $ leftmost
    [ updated initialLoggedIn
    , loginEvent
    ]
  pure $ AppState loginStateDyn triggerLogin

sendRequest :: ( MonadJSM
               ( Performable m )
               , PerformEvent t m
               , TriggerEvent t m
               , ToJSON a)
            => Text -> Event t a -> m (Event t XhrResponse)
sendRequest path event = performRequestAsync $ fmap (postJson ("http://localhost:8000/" <> path)) event

