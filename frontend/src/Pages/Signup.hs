{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pages.Signup where

import           Common.Api
import           Common.Route
import           Control.Monad          (void)
import           Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
import           Safe                   (fromJustDef)

signupPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m => RoutedT t () m ()
signupPage = do
  el "hi" $ text "Signup page"
  el "div" $ mdo

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

      sndPasswordEl <- el "div" $ do
        el "label" $ text "Re-Enter Password: "
        inputElement $ def
                     & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "password")
                     & inputElementConfig_setValue .~ clearEv

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

      let signupData = tag ( current $ LoginReq
                                   <$> _inputElement_value usernameEl
                                   <*> ((decodeUtf8 . hashForSending) <$> _inputElement_value passwordEl)
                          ) loginEvent

      void $ prerender (pure ()) $ do
        loginResponseEvent <- performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> "newuser") signupData
        let getResponse = fmap (fromJustDef "" . _xhrResponse_responseText) loginResponseEvent
        dynText =<< holdDyn "Loading ...." (leftmost [getResponse])
        pure ()
    pure ()
  pure ()

