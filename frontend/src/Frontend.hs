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
import           Control.Monad               ((>=>))
import           General.Functions
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Pages.Login
import           Pages.Main
import           Pages.Signup
import           Reflex.Dom.Core

initial :: ObeliskWidget t (R FrontendRoute) m => m (Dynamic t (Maybe (Auth, User)))
initial = do
  nestedDyn <- prerender
    (pure $ constDyn Nothing)
    (do
      cookieDyn       <- cookieWatcher
      let parsedDyn    = fmap (statusCookieMaybe >=> parseCookie) cookieDyn
      firstParsedE    <- headE $ fmapMaybe id (updated parsedDyn)
      oneAndDoneDyn   <- holdDyn Nothing (Just <$> firstParsedE)
      pure oneAndDoneDyn
    )
  pure $ flattenDyn nestedDyn

buildAppState :: forall t m. ObeliskWidget t (R FrontendRoute) m => m (AppState t)
buildAppState = do
  initialLoggedIn <- initial
  (loginEvent, triggerLogin) <- newTriggerEvent
  loginStateDyn <- holdDyn Nothing $ leftmost
    [ updated initialLoggedIn
    , loginEvent
    ]
  pure $ AppState loginStateDyn triggerLogin

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Twitter Clone"
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

