{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module General.Buttons ( logoutButton
                       , LogInAndOut (JustOut, InAndOut)
                       ) where

import           Common.Route
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (ToJSON)
import           Data.Maybe                  (isJust)
import           Data.Text                   (isInfixOf)
import           General.Functions
import           Language.Javascript.JSaddle (MonadJSM, liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
import           Safe                        (fromJustDef)

logoutEvent :: ( MonadJSM (Performable m)
          , PerformEvent t m, TriggerEvent t m
          , ToJSON a
          ) => Event t a -> m (Event t XhrResponse)
logoutEvent logoutClick = do
    performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> "logout") logoutClick

data LogInAndOut = JustOut | InAndOut deriving stock Eq

logoutButton :: ObeliskWidget t (R FrontendRoute) m  => LogInAndOut -> AppState t -> RoutedT t () m ()
logoutButton logInAndOut appState = el "div" $ do
  _ <- prerender (pure ()) $ do
    let showButton = isJust <$> appLoggedIn appState

    dyn_ $ ffor showButton $ \showBtn ->
      if showBtn
      then do
        _ <- prerender (pure ()) $ do
          logoutClick <- button "Logout"
          logoutResponseEvent <- logoutEvent logoutClick
          let getResponse = fmap (fromJustDef "" . _xhrResponse_responseText) logoutResponseEvent
              isSuccess   = isInfixOf "Success"
              failureResp = ffilter (not . isSuccess) getResponse

          performEvent_ $ ffor failureResp $ \_ -> liftJSM $ do
            cookieText <- getCookies
            let parsed = statusCookieMaybe cookieText >>= parseCookie
            liftIO $ triggerLoggedIn appState parsed
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


