{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Pages.Signup where

import           Common.Api
import           Common.Route
import           Control.Monad               (void)
import           Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import           General.Buttons
import           General.Functions
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
import           Safe                        (fromJustDef)

signupPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m => AppState t -> RoutedT t () m ()
signupPage appState = mdo
  logoutButton InAndOut appState
  el "hi" $ text "Signup page"
  (formEl, _) <- elAttr' "form" ("onsubmit" =: "return false;") $ do
    rec
      usernameEl <- el "div" $ do
        el "label" $ text "Username: "
        textBox NotPassword clearEv Nothing

      passwordEl <- el "div" $ do
        el "label" $ text "Password: "
        textBox Password clearEv Nothing

      sndPasswordEl <- el "div" $ do
        el "label" $ text "Re-Enter Password: "
        textBox Password clearEv Nothing

      void $ button "Sign Up"

      let submitEv = domEvent Submit formEl
          sameValue = (==) <$> _inputElement_value passwordEl <*> _inputElement_value sndPasswordEl

          bothFilledDyn = (&&) <$> ((&&)
            <$> fmap (not . T.null) (_inputElement_value usernameEl)
            <*> fmap (not . T.null) (_inputElement_value passwordEl))
            <*> fmap (not . T.null) (_inputElement_value sndPasswordEl)

          nonEmptyAndSameValue = (&&) <$> sameValue <*> bothFilledDyn

          loginEvent = gate (current nonEmptyAndSameValue) submitEv

          clearEv = "" <$ loginEvent

      dynText $ ffor (zipDyn sameValue bothFilledDyn) $ \(isSame, isNotEmpty) ->
        if isNotEmpty
        then
          if isSame
          then "✅ Values match"
          else "❌ Values do not match"
        else ""

      let signupData = tag ( current $ UserDetailsReq
                                    <$> _inputElement_value usernameEl
                                    <*> ((decodeUtf8 . hashForSending) <$> _inputElement_value passwordEl)
                           ) loginEvent

      void $ prerender (pure ()) $ do
        resp <- sendRequest "ssignup" signupData
        let txtEv   = fmap (fromJustDef "" . _xhrResponse_responseText) resp
            success = ffilter ("Success" `T.isInfixOf`) txtEv
            failure = ffilter (not . ("Success" `T.isInfixOf`)) txtEv

        setRoute ((FrontendRoute_Login :/ ()) <$ success)

        failureDyn <- holdDyn "" failure
        el "div" $ dynText failureDyn
        pure ()
      pure ()
    pure ()
  pure ()

