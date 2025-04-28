{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}

module General.Buttons (logoutButton) where

import           Common.Route
import           Data.Aeson                  (ToJSON)
import           General.Functions
import           Language.Javascript.JSaddle (MonadJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core

logoutEvent :: ( MonadJSM (Performable m)
          , PerformEvent t m, TriggerEvent t m
          , ToJSON a
          ) => Event t a -> m (Event t XhrResponse)
logoutEvent logoutClick = do
    performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> "logout") logoutClick

logoutButton :: ObeliskWidget t (R FrontendRoute) m  => RoutedT t () m ()
logoutButton = el "div" $ do
  _ <- prerender (pure ()) $ do
    cookieDyn <- cookieWatcher
    let showButton = statusCookie cookieDyn

    dyn_ $ ffor showButton $ \showBtn ->
      if showBtn
      then do
        _ <- prerender (pure ()) $ do
          logoutClick <- button "Logout"
          _ <- logoutEvent logoutClick
          pure ()
        pure ()
      else blank
  pure ()

