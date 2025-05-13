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
import           Data.Text                   as T (isInfixOf, null)
import           Data.Text.Encoding          (decodeUtf8)
import           General.Buttons
import           General.Functions
import           Language.Javascript.JSaddle (liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
import           Safe                        (fromJustDef)

loginPage :: ObeliskWidget t (R FrontendRoute) m  => AppState t -> RoutedT t () m ()
loginPage appState = mdo
  logoutButton JustOut appState
  el "h1" $ text "LOGIN PAGE"

  (formEl, _) <- elAttr' "form" ("onsubmit" =: "return false;") $ do
    rec
      usernameEl <- el "div" $ do
        el "label" $ text "Username: "
        inputElement $ def
          & inputElementConfig_setValue .~ clearEv

      passwordEl <- el "div" $ do
        el "label" $ text "Password: "
        inputElement $ def
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "password")
          & inputElementConfig_setValue .~ clearEv

      void $ button "Login"

      let submitEv = domEvent Submit formEl

          bothFilledDyn = (&&)
            <$> fmap (not . T.null) (_inputElement_value usernameEl)
            <*> fmap (not . T.null) (_inputElement_value passwordEl)

          loginEvent = gate (current bothFilledDyn) submitEv

          clearEv = "" <$ loginEvent

      let loginReqEv = tagPromptlyDyn (LoginReq
                            <$> _inputElement_value usernameEl
                            <*> (decodeUtf8 . hashForSending <$> _inputElement_value passwordEl)
                          ) loginEvent

      void $ prerender (pure ()) $ do
        resp <- performRequestAsync $ fmap (postJson ("http://localhost:8000/login")) loginReqEv
        let txtEv   = fmap (fromJustDef "" . _xhrResponse_responseText) resp
            success = ffilter ("Success" `T.isInfixOf`) txtEv
            failure = ffilter (not . ("Success" `T.isInfixOf`)) txtEv

        performEvent_ $ ffor success $ \_ -> liftJSM $ do
          ct <- getCookies
          let mParsed = statusCookieMaybe ct >>= parseCookie
          liftIO $ triggerLoggedIn appState mParsed
        setRoute ((FrontendRoute_Main :/ ()) <$ success)

        failureDyn <- holdDyn "" failure
        el "div" $ dynText failureDyn
        pure ()
    pure ()
  pure ()

