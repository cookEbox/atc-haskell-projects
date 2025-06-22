{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module General.Buttons ( loginControlButton
                       , LoggedOutButtons 
                         ( LoginAndMain
                         , LoginAndSignup
                         , SignupAndMain 
                         )
                       , Password (Password, NotPassword)
                       , Hideable (Hideable, Persistent)
                       , textBox
                       ) where

import           Common.Route
import           Control.Monad               (void)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Map.Strict             (Map)
import           Data.Maybe                  (isJust, fromMaybe)
import           Data.Text                   (isInfixOf, Text)
import           General.Functions
import           Language.Javascript.JSaddle (liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core

data LoggedOutButtons 
  = LoginAndMain 
  | LoginAndSignup 
  | SignupAndMain
  deriving stock Eq

logOut :: ( SetRoute t (R FrontendRoute) (Client m)
          , Monad m
          , Prerender t m
          ) => AppState t -> m ()
logOut appState = do
  void $ prerender (pure ()) $ do
    logoutClickEv <- button "Logout"
    logoutResponseEv <- sendRequest "slogout" logoutClickEv
    let responseTxt   = fromMaybe "" . _xhrResponse_responseText
        getResponseEv = fmap responseTxt logoutResponseEv
        isSuccess     = isInfixOf "Success"
        failureRespEv = ffilter (not . isSuccess) getResponseEv
    performEvent_ $ ffor failureRespEv $ \_ -> liftJSM $ do
      cookieText <- getCookies
      let parsed = statusCookieMaybe cookieText >>= parseCookie
      liftIO $ triggerLoggedIn appState parsed

loginControlButton :: ObeliskWidget t (R FrontendRoute) m  
                   => LoggedOutButtons 
                   -> AppState t 
                   -> RoutedT t () m ()
loginControlButton loggedOutButtons appState = el "div" $ do
  void $ prerender (pure ()) $ do
    let showButton = isJust <$> appLoggedIn appState
    dyn_ $ ffor showButton $ \showBtn ->
      if showBtn
      then logOut appState
      else 
        case loggedOutButtons of 
        LoginAndSignup -> do loginPageButton
                             signUpPageButton
        LoginAndMain   -> do loginPageButton 
                             mainPageButton
        SignupAndMain  -> do signUpPageButton
                             mainPageButton

mainPageButton :: ( DomBuilder t m , SetRoute t (R FrontendRoute) m) => m () 
mainPageButton = do 
  homeClickEv <- button "Home"
  setRoute $ (FrontendRoute_Main :/ ()) <$ homeClickEv

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
textBox NotPassword clearEv Persistent 
  = inputElement $ def & inputElementConfig_setValue .~ clearEv
textBox NotPassword clearEv (Hideable hideEv) 
  = inputElement $ def 
                 & inputElementConfig_setValue .~ clearEv
                 & inputElementConfig_elementConfig 
                   . elementConfig_initialAttributes .~ ("disabled" =: "true")
                 & inputElementConfig_elementConfig 
                   . elementConfig_modifyAttributes .~ hideEv
                 & initialAttributes .~ ("maxlength" =: "280")
textBox Password clearEv _ 
  = inputElement $ def
                 & inputElementConfig_elementConfig 
                   . elementConfig_initialAttributes .~ ("type" =: "password")
                 & inputElementConfig_setValue .~ clearEv
                 & initialAttributes .~ ("maxlength" =: "64")

