{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Main where

import           Common.Api
import           Common.Route
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   as T
import           General.Buttons
import           General.Functions
import           Language.Javascript.JSaddle (liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
import           Safe                        (fromJustDef)

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
  input <- inputElement def
  submitBtn <- button "📨"

  _ <- prerender (pure ()) $ do
    nameAndAuthEventMaybe <- selectCookies submitBtn
    let nameAndAuthEvent = fromMaybe ("here","and here") <$> nameAndAuthEventMaybe

    let msgEvent = tagPromptlyDyn (_inputElement_value input) submitBtn
    msgDyn <- holdDyn "" msgEvent

    let reqEvent = attachWith (\msg (auth,username) -> MessageReq username msg auth) (current msgDyn) nameAndAuthEvent

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

