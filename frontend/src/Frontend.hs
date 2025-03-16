{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Frontend where

import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Data.Text.Encoding      (decodeUtf8)
import           Reflex.Dom.Core
import           Common.Route
import           Data.Text                as T
import           Safe                     (fromJustDef)

userName :: T.Text
userName = "Nick"

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Twitter Clone"
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
    subRoute_ $ \case
      FrontendRoute_Main -> do
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

      FrontendRoute_Login -> do
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

    pure ()
  }

