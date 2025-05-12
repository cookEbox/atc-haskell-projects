{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Main where

import           Common.Api
import           Common.Route
import           Control.Monad               (void)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   as T
import           General.Buttons
import           General.Functions
import           Language.Javascript.JSaddle (liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core

selectCookies :: MonadWidget t m => Event t () -> m (Event t (Maybe (Text, Text)))
selectCookies click = do
  authEvent <- performEvent $ ffor click $ \_ -> do
    cookieText <- liftJSM getCookies
    pure (parseCookie cookieText)
  pure authEvent

mainPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m => AppState t -> RoutedT t () m ()
mainPage appState = do
  logoutButton InAndOut appState
  el "h1" $ text "Obelisk Echo App"
  el "p" $ text "Enter text and press submit:"

  rec
    respTextDyn <- prerender (pure never) $ do
      nameAuthEvMaybe <- selectCookies submitBtn
      let nameAuthEv = fromMaybe ("", "") <$> nameAuthEvMaybe

      let msgEv = tagPromptlyDyn (_inputElement_value input) submitBtn
      msgDyn <- holdDyn "" msgEv
      let reqEv = attachPromptlyDynWith
                    (\msg (auth, user) -> MessageReq user msg auth)
                    msgDyn
                    nameAuthEv

      postbuild <- getPostBuild
      initResp <- performRequestAsync $ fmap (postJson ("http://localhost:8000/" <> get)) postbuild
      let initText = fmap (fromMaybe "" . _xhrResponse_responseText) initResp
      postResp <- performRequestAsync $ fmap (postJson ("http://localhost:8000/" <> post)) reqEv
      let triggerGet = void postResp
      getResp <- performRequestAsync $ fmap (postJson ("http://localhost:8000/" <> get)) triggerGet
      let getText = fmap (fromMaybe "" . _xhrResponse_responseText) getResp
      pure $ leftmost [initText, getText]

    let respTextEvent = switchDyn respTextDyn
        clearEvent = "" <$ respTextEvent

    (input, submitBtn) <- el "div" $ do
      ie <- inputElement $ def & inputElementConfig_setValue .~ clearEvent
      sb <- button "📨"
      pure (ie, sb)

    displayDyn <- holdDyn "Loading...." respTextEvent
    el "div" $ dynText displayDyn
  pure ()

