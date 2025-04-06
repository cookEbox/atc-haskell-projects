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

module Pages.Main where

import           Common.Route
import           Common.Api
import           Data.Text                as T
import           Obelisk.Frontend
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

