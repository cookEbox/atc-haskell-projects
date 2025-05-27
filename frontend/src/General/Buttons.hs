{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module General.Buttons ( logoutButton
                       , LogInAndOut (JustOut, InAndOut)
                       , Password (Password, NotPassword)
                       , textBox
                       ) where

import           Common.Route
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (ToJSON)
import           Data.Map.Strict             (Map)
import           Data.Maybe                  (isJust, fromMaybe)
import           Data.Text                   (isInfixOf, Text)
import           General.Functions
import           Language.Javascript.JSaddle (MonadJSM, liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core

logoutEvent :: ( MonadJSM (Performable m)
          , PerformEvent t m, TriggerEvent t m
          , ToJSON a
          ) => Event t a -> m (Event t XhrResponse)
logoutEvent logoutClick = do
    performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> "slogout") logoutClick

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
          let getResponse = fmap (fromMaybe "" . _xhrResponse_responseText) logoutResponseEvent
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

data Password = Password | NotPassword deriving stock Eq

textBox :: DomBuilder t m => Password -> Event t Text -> Maybe (Event t (Map AttributeName (Maybe Text))) -> m (InputElement EventResult (DomBuilderSpace m) t)
textBox NotPassword event Nothing = inputElement $ def & inputElementConfig_setValue .~ event 
textBox NotPassword event (Just event2) = 
  inputElement $ def 
               & inputElementConfig_setValue .~ event 
               & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ event2
textBox Password event _ = 
  inputElement $ def
               & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "password")
               & inputElementConfig_setValue .~ event

