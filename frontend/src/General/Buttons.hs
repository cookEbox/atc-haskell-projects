{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module General.Buttons ( loginControlButton
                       , LoggedOutControlButtons (Login, LoginAndSignup, Signup)
                       , Password (Password, NotPassword)
                       , Hideable (Hideable, Persistent)
                       , textBox
                       ) where

import           Common.Route
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (ToJSON)
import           Data.Map.Strict             (Map)
import           Data.Maybe                  (isJust, fromMaybe)
import           Data.Text                   (isInfixOf, Text)
import           General.Functions
import           Language.Javascript.JSaddle (MonadJSM, liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core

data LoggedOutControlButtons 
  = Login 
  | LoginAndSignup 
  | Signup
  deriving stock Eq

logoutEv :: ( MonadJSM (Performable m)
            , PerformEvent t m, TriggerEvent t m
            , ToJSON a
            ) => Event t a -> m (Event t XhrResponse)
logoutEv logoutClickEv = sendRequest "slogout" logoutClickEv

loginControlButton :: ObeliskWidget t (R FrontendRoute) m  
                   => LoggedOutControlButtons 
                   -> AppState t 
                   -> RoutedT t () m ()
loginControlButton logInAndOut appState = el "div" $ do
  _ <- prerender (pure ()) $ do
    let showButton = isJust <$> appLoggedIn appState

    dyn_ $ ffor showButton $ \showBtn ->
      if showBtn
      then do
        _ <- prerender (pure ()) $ do
          logoutClickEv <- button "Logout"
          logoutResponseEv <- logoutEv logoutClickEv
          let responseTxt   = fromMaybe "" . _xhrResponse_responseText
              getResponseEv = fmap responseTxt logoutResponseEv
              isSuccess     = isInfixOf "Success"
              failureRespEv = ffilter (not . isSuccess) getResponseEv

          performEvent_ $ ffor failureRespEv $ \_ -> liftJSM $ do
            cookieText <- getCookies
            let parsed = statusCookieMaybe cookieText >>= parseCookie
            liftIO $ triggerLoggedIn appState parsed
          pure ()
        pure ()
      else case logInAndOut of 
        LoginAndSignup -> do loginPageButton
                             signUpPageButton
        Login          -> loginPageButton 
        Signup         -> signUpPageButton
  pure ()

signUpPageButton :: ( DomBuilder t m , SetRoute t (R FrontendRoute) m) => m () 
signUpPageButton = do 
  signupClickEv <- button "Sign Up"
  setRoute $ (FrontendRoute_Signup :/ ()) <$ signupClickEv

loginPageButton :: ( DomBuilder t m , SetRoute t (R FrontendRoute) m) => m ()
loginPageButton = do
  loginClickEv <- button "Login"
  setRoute $ (FrontendRoute_Login :/ ()) <$ loginClickEv

data Password 
  = Password 
  | NotPassword 
  deriving stock Eq

data Hideable t 
  = Hideable (Event t (Map AttributeName (Maybe Text))) 
  | Persistent

textBox :: DomBuilder t m 
        => Password 
        -> Event t Text 
        -> Hideable t 
        -> m (InputElement EventResult (DomBuilderSpace m) t)
textBox NotPassword event Persistent 
  = inputElement $ def & inputElementConfig_setValue .~ event 
textBox NotPassword event (Hideable event2) 
  = inputElement $ def 
                 & inputElementConfig_setValue .~ event 
                 & inputElementConfig_elementConfig 
                 . elementConfig_modifyAttributes .~ event2
textBox Password event _ 
  = inputElement $ def
                 & inputElementConfig_elementConfig 
                 . elementConfig_initialAttributes .~ ("type" =: "password")
                 & inputElementConfig_setValue .~ event

