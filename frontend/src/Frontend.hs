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

module Frontend where

import           Common.Route
import           Data.Aeson               (decodeStrict)
import           Data.Text.Encoding       (encodeUtf8)
import           General.Functions
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Pages.Login
import           Pages.Main
import           Pages.Signup
import           Reflex.Dom.Core

buildAppState :: ObeliskWidget t (R FrontendRoute) m => m (AppState t)
buildAppState = mdo
  (refreshEv, triggerRefresh) <- newTriggerEvent
  nestedDyn <- prerender
    (pure $ constDyn Nothing) $ do
      onLoad <- getPostBuild
      let reloadEv = leftmost [onLoad, refreshEv]
      respEv <- sendRequest "auth" reloadEv
      holdDyn Nothing $ fmap decodeUserInfo (_xhrResponse_responseText <$> respEv)
  pure $ AppState (flattenDyn nestedDyn) triggerRefresh
  where
    decodeUserInfo = (>>= decodeStrict . encodeUtf8)


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Twitter Clone"
      elAttr "link"
        (  "rel"  =: "stylesheet"
        <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
        <> "integrity" =: "sha512-iecdLmaskl7CVkqkXNQ/ZH/XLlvWZOJyj7Yy7tcenmpD1ypASozpmT/E0iPtmFIB46ZmdtAc9eNBvH0H/ZpiBw=="
        <> "crossorigin" =: "anonymous"
        <> "referrerpolicy" =: "no-referrer"
        ) blank
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
    appState <- buildAppState
    subRoute_ $ \case
      FrontendRoute_Main -> mainPage appState
      FrontendRoute_Login -> loginPage appState
      FrontendRoute_Signup -> signupPage appState
    pure ()
  }

