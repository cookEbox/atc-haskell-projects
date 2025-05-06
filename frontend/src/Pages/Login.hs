{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}

module Pages.Login where

import           Common.Api
import           Common.Route
import           Data.Text.Encoding          (decodeUtf8)
import           General.Buttons
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
import           Safe                        (fromJustDef)

loginPage :: ObeliskWidget t (R FrontendRoute) m  => RoutedT t () m ()
loginPage = do
  logoutButton JustOut

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

