{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Pages.Main where

import           Common.Api
import           Common.Route
import           Data.Text                   as T
import Data.Maybe (fromMaybe)
import           General.Functions
import           Language.Javascript.JSaddle (liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
import           Safe                        (fromJustDef)

userName :: T.Text
userName = "Dave"

authCookiesEvent :: MonadWidget t m => Event t () -> m (Event t (Maybe Text))
authCookiesEvent click = do
  authEvent <- performEvent $ ffor click $ \_ -> do
    cookieText <- liftJSM getCookies
    pure (parseAuth cookieText)

  pure authEvent

mainPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m => RoutedT t () m ()
mainPage = do
  el "h1" $ text "Obelisk Echo App"
  el "p" $ text "Enter text and press submit:"
  input <- inputElement def
  submitBtn <- button "Send to Backend"

  _ <- prerender (pure ()) $ do
    authEventMaybe <- authCookiesEvent submitBtn
    let authEvent = fromMaybe "" <$> authEventMaybe

    let msgEvent = tagPromptlyDyn (_inputElement_value input) submitBtn
    msgDyn <- holdDyn "" msgEvent
    let msgBehaviour = current msgDyn

    let reqEvent = attachWith (\msg auth -> MessageReq userName msg auth) msgBehaviour authEvent

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

