{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Frontend where

-- import Control.Lens ((^.))
import           Control.Monad
-- import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text                as T
-- import qualified Data.Text.Lazy.Encoding as T
-- import Language.Javascript.JSaddle (MonadJSM, liftJSM, js, js1, jsg)
-- import Control.Monad.Fix (MonadFix)
import           Obelisk.Frontend
-- import Obelisk.Configs
import           Obelisk.Generated.Static
import           Obelisk.Route

import           Reflex.Dom.Core

-- import Common.Api
import           Common.Route
import           Data.Aeson as A
import Safe (fromJustDef)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Twitter Clone"
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
    el "h1" $ text "Obelisk Echo App"
    el "p" $ text "Enter text and press submit:"

    input <- inputElement def
    submitBtn <- button "Send to Backend"
    let reqEvent = tag (current $ fmap (\t -> MessageReq t) $ _inputElement_value input) submitBtn
    void $ prerender (pure ()) $ do
      respEvent <- performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> echoPath) reqEvent
      let respText = fmap (MessageResp . fromJustDef "" . _xhrResponse_responseText) respEvent
      dynText =<< holdDyn "Waiting for response..." (fmap responseMsg respText)
      pure ()

    -- el "h1" $ text "Message Board"
    -- rec
    --   void $ el "div" $ simpleList dText $ \d -> el "p" $ dynText d
    --   let clearEvent = "" <$ eClick
    --       inputConfig = def & inputElementConfig_setValue .~ clearEvent
    --   t <- inputElement inputConfig
    --   eClick <- button "Add Text"
    --   let eText = tag (current $ _inputElement_value t) eClick
    --   dText <- foldDyn (\new old -> old ++ [new]) [] eText
    return ()
  }

backendReq :: ToJSON a => a -> XhrRequest T.Text
backendReq inputText = postJson ("http://localhost:8000/" <> echoPath) inputText

