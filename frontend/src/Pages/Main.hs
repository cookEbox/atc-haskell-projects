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
mainPage appState = mdo
  logoutButton InAndOut appState
  el "h1" $ text "Twitter App"
  el "p" $ text "Enter text and press submit:"

  (formEl, _) <- elAttr' "form" ("onsubmit" =: "return false;") $ el "div" $ do
    rec
      respTextDyn <- prerender (pure never) $ do
        nameAuthEvMaybe <- selectCookies loginEv
        let nameAuthEv = fromMaybe ("", "") <$> nameAuthEvMaybe

        let msgEv = tagPromptlyDyn (_inputElement_value inputEl) loginEv
        msgDyn <- holdDyn "" msgEv
        let reqEv = attachPromptlyDynWith
                      (\msg (auth, user) -> MessageReq user msg auth)
                      msgDyn
                      nameAuthEv

        postbuild <- getPostBuild
        initEv <- performRequestAsync $ fmap (postJson ("http://localhost:8000/" <> get)) postbuild
        let initText = fmap (fromMaybe "" . _xhrResponse_responseText) initEv
        postEv <- performRequestAsync $ fmap (postJson ("http://localhost:8000/" <> post)) reqEv
        let triggerGet = void postEv
        getEv <- performRequestAsync $ fmap (postJson ("http://localhost:8000/" <> get)) triggerGet
        let getText = fmap (fromMaybe "" . _xhrResponse_responseText) getEv
        pure $ leftmost [initText, getText]

      let respTextEv = switchDyn respTextDyn
          enterEv = domEvent Submit formEl
          nonEmpty = not . T.null <$> _inputElement_value inputEl
          loginEv = gate (current nonEmpty) enterEv
          clearEv = "" <$ loginEv

      inputEl <- el "div" $ do
        ie <- inputElement $ def & inputElementConfig_setValue .~ clearEv
        void $ button "📨"
        pure ie

      displayDyn <- holdDyn "Loading...." respTextEv
      el "div" $ dynText displayDyn
    pure ()
  pure ()

