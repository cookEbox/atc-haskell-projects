{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Login where

import           Common.Api
import           Common.Route
import           Control.Monad               (void)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (ToJSON)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   as T (Text, isInfixOf, null)
import           Data.Text.Encoding          (decodeUtf8)
import           General.Buttons
import           General.Functions
import           Language.Javascript.JSaddle (liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core

logIn :: ( ToJSON a, SetRoute t (R FrontendRoute) (Client m)
          , Monad m
          , Prerender t m
          ) => Event t a -> AppState t -> m (Dynamic t Text)
logIn loginDataEv appState = do
  nestedDyn <- prerender (pure $ constDyn "") $ do
    resp <- sendRequest "slogin" loginDataEv
    let txtEv   = fmap (fromMaybe "" . _xhrResponse_responseText) resp
        success = ffilter ("Success" `T.isInfixOf`) txtEv
        failure = ffilter (not . ("Success" `T.isInfixOf`)) txtEv

    performEvent_ $ ffor success $ \_ -> liftJSM $ do
      ct <- getCookies
      let mParsed = statusCookieMaybe ct >>= parseCookie
      liftIO $ triggerLoggedIn appState mParsed

    setRoute ((FrontendRoute_Main :/ ()) <$ success)
    holdDyn "" failure
  pure $ flattenDyn nestedDyn

loginPage :: ObeliskWidget t (R FrontendRoute) m
          => AppState t -> RoutedT t () m ()
loginPage appState = mdo
  loginControlButton Signup appState
  el "h1" $ text "LOGIN PAGE"
  (formEl, _) <- elAttr' "form" ("onsubmit" =: "return false;") $ do
    rec
      usernameEl <- el "div" $ do
        el "label" $ text "Username: "
        textBox NotPassword clearEv Persistent

      passwordEl <- el "div" $ do
        el "label" $ text "Password: "
        textBox Password clearEv Persistent

      void $ button "Login"

      let submitEv          = domEvent Submit formEl
          usernameDyn       = _inputElement_value usernameEl
          passwordDyn       = _inputElement_value passwordEl
          clearEv           = "" <$ loginEv
          hashedPasswordDyn = decodeUtf8 . hashForSending <$> passwordDyn
          bothFilledDyn     = (&&)
                              <$> fmap (not . T.null) usernameDyn
                              <*> fmap (not . T.null) passwordDyn
          loginEv           = gate (current bothFilledDyn) submitEv
          loginDataEv       = tagger usernameDyn hashedPasswordDyn loginEv

      failureDyn <- logIn loginDataEv appState
      el "div" $ dynText failureDyn
    pure ()
  pure ()

