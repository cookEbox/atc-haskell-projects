{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}

module General.Buttons ( logoutButton 
                       , LogInAndOut (JustOut, InAndOut) 
                       ) where

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

data LogInAndOut = JustOut | InAndOut deriving stock Eq

logoutButton :: ObeliskWidget t (R FrontendRoute) m  => LogInAndOut -> RoutedT t () m ()
logoutButton logInAndOut = el "div" $ do
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
      else if logInAndOut == InAndOut 
           then loginPageButton 
           else blank
  pure ()

loginPageButton :: ( DomBuilder t m , SetRoute t (R FrontendRoute) m) => m ()
loginPageButton = do
  loginPageClick <- button "Login"
  setRoute $ (FrontendRoute_Login :/ ()) <$ loginPageClick

