{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Profile where

import           Common.Api
import           Common.Route
import           Control.Monad              (void)
import           Data.Maybe                 (fromMaybe)
import           General.Buttons
import           General.Elements
import           General.Functions
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Pages.ProfileShared.Parser
import           Pages.ProfileShared.GetProfile
import           Reflex.Dom.Core            hiding (el, elAttr, elAttr')

isJustEqual :: Eq a => Maybe a -> Maybe a -> Bool
isJustEqual (Just a) (Just b) = a == b
isJustEqual _        _        = False

showEditButton :: ObeliskWidget t (R FrontendRoute) m
               => AppState t
               -> RoutedT t () m ()
showEditButton appState = el_ DIV $ do
  void $ prerender (pure ()) $ do
  let showButton = isJustEqual <$> (fmap uiId <$> appLoggedIn appState)
                               <*> profileUidDyn appState
  dyn_ $ ffor showButton $ \showBtn ->
    if showBtn
    then editProfileButton
    else pure ()

editProfileButton :: (DomBuilder t m , SetRoute t (R FrontendRoute) m) => m ()
editProfileButton = do
  editClickEv <- button "Edit"
  setRoute $ (FrontendRoute_ProfileUp :/ ()) <$ editClickEv

profile :: ObeliskWidget t (R FrontendRoute) m
        => AppState t -> RoutedT t () m ()
profile appState = do
  elClass_ DIV "profile-page" $ do
    profileDyn  <- getProfileDyn appState
    dyn_ $ ffor profileDyn $ \profMb -> do
      case profMb of
        Nothing ->
          elClass_ DIV "profile-container" $ do
            mainPageButton
            loginControlButton LoginAndMain appState
            el_ H1 $ text "PROFILE PAGE"
            elClass_ DIV "not-found" $
              text "User not found"

        Just prof -> do
          elClass_ DIV "profile-container" $ do
            elClass_ DIV "profile-buttons" $ do
              showEditButton appState
              mainPageButton
              loginControlButton LoginAndMain appState
            el_ H1 $ text "PROFILE PAGE"
            elClass_ DIV "profile-field" $ do
              elClass_ DIV "profile-label" $ text "Username: "
              el_ DIV $ text (fromMaybe "" $ prName prof)

            elClass_ DIV "profile-field" $ do
              elClass_ DIV "profile-label" $ text "DOB (dd/mm/yyyy): "
              el_ DIV $ text (fromMaybe "" $ prDOB prof >>= printDate)

            elClass_ DIV "profile-field" $ do
              elClass_ DIV "profile-label" $ text "City/County: "
              el_ DIV $ text (fromMaybe "" $ prLocation prof)

            elClass_ DIV "profile-field" $ do
              elClass_ DIV "profile-label" $ text "Hobbies: "
              el_ DIV $ text (fromMaybe "" $ prHobbies prof)

            elClass_ DIV "profile-field" $ do
              elClass_ DIV "profile-label" $ text "Bio: "
              el_ DIV $ text (fromMaybe "" $ prBio prof)
