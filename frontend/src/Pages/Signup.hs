{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE KindSignatures        #-}

module Pages.Signup where

import           Common.Route
import           Common.Api
import           Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
import           Safe                     (fromJustDef)

signupPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m => RoutedT t () m () 
signupPage = do 
  el "hi" $ text "Signup page"
  el "div" $ do
    username <- el "div" $ do
      el "label" $ text "Username: "
      inputElement def

    password <- el "div" $ do
      el "label" $ text "Password: "
      inputElement $ def
                   & inputElementConfig_elementConfig . elementConfig_initialAttributes
                   .~ ("type" =: "password")

    sndPassword <- el "div" $ do
      el "label" $ text "Re-Enter Password: "
      inputElement $ def
                   & inputElementConfig_elementConfig . elementConfig_initialAttributes
                   .~ ("type" =: "password")

    let sameValue = (==) <$> _inputElement_value password <*> _inputElement_value sndPassword
        notEmtpyPassword =  (\x -> T.length x /= 0) <$> _inputElement_value password 
        notEmtpySndPassword =  (\x -> T.length x /= 0) <$> _inputElement_value sndPassword 
        notEmptyPasswords = (&&) <$> notEmtpyPassword <*> notEmtpySndPassword

    dynText $ ffor (zipDyn sameValue notEmptyPasswords) $ \(isSame, isNotEmpty) ->
      if isNotEmpty 
      then 
        if isSame 
        then "✅ Values match"
        else "❌ Values do not match"
      else ""

    signupClick <- button "Sign Up"

    let signupData = tag ( current $ LoginReq
                                 <$> _inputElement_value username
                                 <*> ((decodeUtf8 . hashForSending) <$> _inputElement_value password)
                        ) signupClick

    _ <- prerender (pure ()) $ do
      loginResponseEvent <- performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> "newuser") signupData
      let getResponse = fmap (fromJustDef "" . _xhrResponse_responseText) loginResponseEvent
      dynText =<< holdDyn "Loading ...." (leftmost [getResponse])
      pure ()
    pure ()

