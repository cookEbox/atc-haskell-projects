{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE KindSignatures        #-}

module Frontend where

import           Common.Route
import           Common.Api
import           Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
import           Safe                     (fromJustDef)

userName :: T.Text
userName = "Nick"

mainPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m => RoutedT t () m ()
mainPage = do
  el "h1" $ text "Obelisk Echo App" 
  el "p" $ text "Enter text and press submit:"
  input <- inputElement def
  submitBtn <- button "Send to Backend"
  let reqEvent = tag (current $ fmap (\t -> MessageReq userName t) $ _inputElement_value input) submitBtn

  _ <- prerender (pure ()) $ do
    postBuild <- getPostBuild
    getInitEvent <- performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> get) postBuild
    let getInitText  = fmap (fromJustDef "" . _xhrResponse_responseText) getInitEvent
    respEvent <- performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> post) reqEvent
    let getReqEvent = (const ()) <$> respEvent

    el "div" $ do
      getRespEvent <- performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> get) getReqEvent
      let getRespText = fmap (fromJustDef "" . _xhrResponse_responseText) getRespEvent
      dynText =<< holdDyn "Loading ...." (leftmost [getInitText, getRespText])
    pure ()
  pure ()

loginPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m => RoutedT t () m () 
loginPage = do
  el "h1" $ text "LOGIN PAGE"
  el "div" $ do
    username <- el "div" $ do
      el "label" $ text "Username: "
      inputElement def

    password <- el "div" $ do
      el "label" $ text "Password: "
      inputElement $ def
                   & inputElementConfig_elementConfig . elementConfig_initialAttributes
                   .~ ("type" =: "password")

    loginClick <- button "Login"

    let loginData = tag ( current $ LoginReq
                                 <$> _inputElement_value username
                                 <*> ((decodeUtf8 . hashForSending) <$> _inputElement_value password)
                        ) loginClick

    _ <- prerender (pure ()) $ do
      loginResponseEvent <- performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> "login") loginData
      let getResponse = fmap (fromJustDef "" . _xhrResponse_responseText) loginResponseEvent
      dynText =<< holdDyn "Loading ...." (leftmost [getResponse])
      pure ()
    pure ()
  pure ()

signupPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m => RoutedT t () m () 
signupPage = do 
  el "hi" $ text "Signup page"
  el "div" $ do
    username <- el "div" $ do
      el "label" $ text "Username: "
      inputElement def

    password <- el "div" $ do
      el "label" $ text "Password: "
      inputElement $ def
                   & inputElementConfig_elementConfig . elementConfig_initialAttributes
                   .~ ("type" =: "password")

    sndPassword <- el "div" $ do
      el "label" $ text "Re-Enter Password: "
      inputElement $ def
                   & inputElementConfig_elementConfig . elementConfig_initialAttributes
                   .~ ("type" =: "password")

    let sameValue = (==) <$> _inputElement_value password <*> _inputElement_value sndPassword
        notEmtpyPassword =  (\x -> T.length x /= 0) <$> _inputElement_value password 
        notEmtpySndPassword =  (\x -> T.length x /= 0) <$> _inputElement_value sndPassword 
        notEmptyPasswords = (&&) <$> notEmtpyPassword <*> notEmtpySndPassword

    dynText $ ffor (zipDyn sameValue notEmptyPasswords) $ \(isSame, isNotEmpty) ->
      if isNotEmpty 
      then 
        if isSame 
        then "✅ Values match"
        else "❌ Values do not match"
      else ""

    signupClick <- button "Sign Up"

    let signupData = tag ( current $ LoginReq
                                 <$> _inputElement_value username
                                 <*> ((decodeUtf8 . hashForSending) <$> _inputElement_value password)
                        ) signupClick

    _ <- prerender (pure ()) $ do
      loginResponseEvent <- performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> "newuser") signupData
      let getResponse = fmap (fromJustDef "" . _xhrResponse_responseText) loginResponseEvent
      dynText =<< holdDyn "Loading ...." (leftmost [getResponse])
      pure ()
    pure ()

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Twitter Clone"
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
    subRoute_ $ \case
      FrontendRoute_Main -> mainPage
      FrontendRoute_Login -> loginPage
      FrontendRoute_Signup -> signupPage
    pure ()
  }

