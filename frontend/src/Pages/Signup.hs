{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Signup (signupPage) where

import           Common.Api
import           Common.Route
import           Data.Aeson             (ToJSON)
import           Data.Maybe             (fromMaybe)
import           Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8)
import           General.Buttons
import           General.Elements
import           General.Functions
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core        hiding (el, elAttr, elAttr')

inputValidator :: (PostBuild t m, DomBuilder t m)
               => Event t a
               -> Dynamic t Text
               -> Dynamic t Text
               -> Dynamic t Text
               -> m (Event t a)
inputValidator submitEv usernameDyn passwordDyn sndPasswordDyn = do
  let sameValueDyn = (==) <$> passwordDyn <*> sndPasswordDyn
      bothFilledDyn =
        (&&) <$> (
          (&&) <$> fmap (not . T.null) usernameDyn
               <*> fmap (not . T.null) passwordDyn
                 )
             <*> fmap (not . T.null) sndPasswordDyn
      nonEmptyAndSameValueDyn = (&&) <$> sameValueDyn <*> bothFilledDyn
      loginEv = gate (current nonEmptyAndSameValueDyn) submitEv

  dynText $ ffor (zipDyn sameValueDyn bothFilledDyn)
          $ \(isSame, isNotEmpty) ->
              if isNotEmpty
              then
                if isSame
                then "✅ Values match"
                else "❌ Values do not match"
              else ""
  pure loginEv

signUp :: ( ToJSON a, SetRoute t (R FrontendRoute) (Client m)
          , Monad m
          , Prerender t m
          ) => Event t a -> m (Dynamic t Text)
signUp signupDataEv = do
  nestedDyn <- prerender (pure $ constDyn "") $ do
    resp <- sendRequest "ssignup" signupDataEv
    let txtEv   = fmap (fromMaybe "" . _xhrResponse_responseText) resp
        success = ffilter ("Success" `T.isInfixOf`) txtEv
        failure = ffilter (not . ("Success" `T.isInfixOf`)) txtEv
    setRoute ((FrontendRoute_Login :/ ()) <$ success)
    holdDyn "" failure
  pure $ flattenDyn nestedDyn

signupPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m
           => AppState t -> RoutedT t () m ()
signupPage appState = do
  elClass_ DIV "login-page" $ do
    elClass_ DIV "login-form" $ mdo
      loginControlButton LoginAndMain appState
      el_ H1 $ text "Sign Up"
      (formEl, _) <- elAttR_ FORM (single $ OnSubmit "return false;") $ do
        rec
          usernameEl <- elClass_ DIV "field-group" $ do
            el_ LABEL $ text "Username: "
            textBox NotPassword clearEv Persistent

          passwordEl <- elClass_ DIV "field-group" $ do
            el_ LABEL $ text "Password: "
            textBox Password clearEv Persistent

          sndPasswordEl <- elClass_ DIV "field-group" $ do
            el_ LABEL $ text "Re-Enter Password: "
            textBox Password clearEv Persistent

          elAttr_ BUTTON (multi [Type "submit", Class "btn"]) $ text "Signup"

          let submitEv       = domEvent Submit formEl
              usernameDyn    = _inputElement_value usernameEl
              passwordDyn    = _inputElement_value passwordEl
              sndPasswordDyn = _inputElement_value sndPasswordEl

          signupEv <- inputValidator submitEv usernameDyn passwordDyn sndPasswordDyn

          let clearEv           = "" <$ signupEv
              hashedPasswordDyn = decodeUtf8 . hashForSending <$> passwordDyn
              signupDataEv      = tagger usernameDyn hashedPasswordDyn signupEv

          failureDyn <- signUp signupDataEv
        elClass_ DIV "error-message" $ dynText failureDyn
      pure ()

