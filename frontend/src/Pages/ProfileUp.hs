{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.ProfileUp where

import           Common.Api
import           Common.Route
import           Data.Aeson                     (ToJSON)
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      as T (Text, isInfixOf, strip)
import           General.Buttons
import           General.Elements
import           General.Functions
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Pages.ProfileShared.GetProfile
import           Pages.ProfileShared.Parser
import           Prelude                        hiding (null)
import           Reflex.Dom.Core                hiding (el, elAttr, elAttr')

fieldToDyn :: Reflex t => InputElement EventResult s t -> Dynamic t (Maybe Text)
fieldToDyn el  = Just . strip <$> _inputElement_value el

profileUpdate :: ( ToJSON a, SetRoute t (R FrontendRoute) (Client m)
                 , Monad m
                 , Prerender t m
                 ) => Event t a -> m (Dynamic t Text)
profileUpdate updateDataEv = do
  nestedDyn <- prerender (pure $ constDyn "") $ do
    resp <- sendRequest "sprofileup" updateDataEv
    let txtEv   = fmap (fromMaybe "" . _xhrResponse_responseText) resp
        success = ffilter ("Success" `T.isInfixOf`) txtEv
        failure = ffilter (not . ("Success" `T.isInfixOf`)) txtEv
    setRoute ((FrontendRoute_Profile :/ ()) <$ success)
    holdDyn "" failure
  pure $ flattenDyn nestedDyn

profilePageButton :: ( DomBuilder t m , SetRoute t (R FrontendRoute) m) => m ()
profilePageButton = do
  profileClickEv <- button "Back"
  setRoute $ (FrontendRoute_Profile :/ ()) <$ profileClickEv

profileSubmit :: ObeliskWidget t (R FrontendRoute) m
              => AppState t -> RoutedT t () m ()
profileSubmit appState = do
  elClass_ DIV "profile-page" $ do
    elClass_ DIV "profile-form" $ mdo
      elClass_ DIV "profile-buttons" $ do
        profilePageButton
        mainPageButton
        loginControlButton LoginAndMain appState
      el_ H1 $ text "UPDATE PROFILE PAGE"
      (formEl, _) <- elAttR_ FORM (single $ OnSubmit "return false;") $ do
        curProfileDyn <- getProfileDyn appState
        dyn_ $ ffor curProfileDyn $ \profMb -> do
          case profMb of
            Nothing -> blank
            Just prof -> do
              rec
                usernameEl <- elClass_ DIV "field-group" $ do
                  el_ LABEL $ text "Username: "
                  textBox NotPassword (printTxt prName) Persistent

                dobEl <- elClass_ DIV "field-group" $ do
                  el_ LABEL $ text "DOB (dd/mm/yyyy): "
                  textBox NotPassword ( fromMaybe ""
                                      . (>>= printDate)
                                      . prDOB <$> initialTxtEv
                                      ) Persistent

                locationEl <- elClass_ DIV "field-group" $ do
                  el_ LABEL $ text "City/County: "
                  textBox NotPassword (printTxt prLocation) Persistent

                hobbiesEl <- elClass_ DIV "field-group" $ do
                  el_ LABEL $ text "Hobbies: "
                  textBox NotPassword (printTxt prHobbies) Persistent

                bioEl <- elClass_ DIV "field-group" $ do
                  el_ LABEL $ text "Bio: "
                  textBox NotPassword (printTxt prBio) Persistent

                elAttr_ BUTTON (multi [Type "submit", Class "btn"]) $ text "Update"

                let submitEv     = domEvent Submit formEl
                    uidDyn       = fmap uiId <$> appLoggedIn appState
                    initialTxtEv = prof <$ postBuildEv
                    printTxt fld = fromMaybe "" . fld <$> initialTxtEv
                    profileDyn   = UserProfile <$> fieldToDyn usernameEl
                                               <*> parsedFieldToDyn dobEl
                                               <*> fieldToDyn locationEl
                                               <*> fieldToDyn hobbiesEl
                                               <*> fieldToDyn bioEl
                                               <*> uidDyn
                    updateDataEv = tag (current profileDyn) submitEv
                postBuildEv <- getPostBuild
                failureDyn <- profileUpdate updateDataEv
              el_ DIV $ dynText failureDyn
      pure ()
