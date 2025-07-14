{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Login (loginPage) where

import           Common.Api
import           Common.Route
import           Control.Monad               (void)
import           Data.Aeson                  (ToJSON)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   as T (Text, isInfixOf, null)
import           Data.Text.Encoding          (decodeUtf8)
import           General.Buttons
import           General.Elements
import           General.Functions
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (el, elAttr, elAttr')

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

    void $ updateState appState success

    setRoute ((FrontendRoute_Main :/ ()) <$ success)
    holdDyn "" failure
  pure $ flattenDyn nestedDyn

loginPage :: ObeliskWidget t (R FrontendRoute) m
          => AppState t -> RoutedT t () m ()
loginPage appState = do
  elClass_ DIV "login-page" $ do
    elClass_ DIV "login-form" $ mdo 
      loginControlButton SignupAndMain appState
      el_ H1 $ text "LOGIN PAGE"
      (formEl, _) <- elAttR_ FORM (single $ OnSubmit "return false;") $ do
        rec
          usernameEl <- elClass_ DIV "field-group" $ do
            el_ LABEL $ text "Username: "
            textBox NotPassword clearEv Persistent

          passwordEl <- elClass_ DIV "field-group" $ do
            el_ LABEL $ text "Password: "
            textBox Password clearEv Persistent

          elAttr_ BUTTON (multi [Type "submit", Class "btn"]) $ text "Login"

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
        el_ DIV $ dynText failureDyn
      pure ()

