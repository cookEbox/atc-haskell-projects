{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend where

-- import Control.Lens ((^.))
import Control.Monad
-- import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
-- import Language.Javascript.JSaddle (liftJSM, js, js1, jsg)
-- import Control.Monad.Fix (MonadFix) 
import Obelisk.Frontend
-- import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

-- import Common.Api
import Common.Route

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Twitter Clone"
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
    el "h1" $ text "Message Board"
    rec
      void $ el "div" $ simpleList dText $ \d -> el "p" $ dynText d
      let clearEvent = "" <$ eClick
          inputConfig = def & inputElementConfig_setValue .~ clearEvent
      t <- inputElement inputConfig 
      eClick <- button "Add Text"
      let eText = tag (current $ _inputElement_value t) eClick
      dText <- foldDyn (\new old -> old ++ [new]) [] eText
    return ()
  }

