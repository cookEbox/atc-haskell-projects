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

module Pages.Login where

import           Common.Api
import           Common.Route
import           Control.Monad.IO.Class      (liftIO)
import           Data.Text                   (Text, isInfixOf)
import           Data.Aeson                  (ToJSON)
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Time.Clock             (getCurrentTime)
import           Language.Javascript.JSaddle (JSM, eval, liftJSM,
                                              strToText, valToStr, MonadJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
import           Safe                        (fromJustDef)

getCookies :: JSM Text
getCookies = strToText <$> (valToStr =<< eval ("document.cookie" :: Text))

cookieWatcher :: (MonadWidget t m) => m (Dynamic t Text)
cookieWatcher = do
  tick <- tickLossy 1 =<< liftIO getCurrentTime
  cookieEvent <- performEvent (liftJSM getCookies <$ tick)
  holdDyn "" cookieEvent

logoutEvent :: ( MonadJSM (Performable m)
          , PerformEvent t m, TriggerEvent t m
          , ToJSON a
          ) => Event t a -> m (Event t XhrResponse)
logoutEvent logoutClick = do 
    performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> "logout") logoutClick

logoutButton :: ObeliskWidget t (R FrontendRoute) m  => RoutedT t () m ()
logoutButton = el "div" $ do
  _ <- prerender (pure ()) $ do 
    cookieDyn <- cookieWatcher
    let showButton = isInfixOf "status=loggedIn" <$> cookieDyn

    dyn_ $ ffor showButton $ \showBtn ->
      if showBtn 
      then do 
        _ <- prerender (pure ()) $ do 
          logoutClick <- button "Logout"
          _ <- logoutEvent logoutClick
          pure ()
        pure () 
      else blank 
  pure ()

loginPage :: ObeliskWidget t (R FrontendRoute) m  => RoutedT t () m ()
loginPage = do
  logoutButton

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

