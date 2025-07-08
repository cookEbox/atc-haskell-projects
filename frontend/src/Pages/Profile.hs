{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Profile where

import           Common.Api
import           Common.Route
import           Control.Monad          (void)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (pack)
import           Data.Time.Calendar     (showGregorian)
import           General.Buttons
import           General.Elements
import           General.Functions
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Prelude                hiding (null)
import           Reflex.Dom.Core        hiding (el, elAttr, elAttr')

getProfile :: (Monad m, Prerender t m)
           => Event t ProfileInfo
           -> m (Dynamic t (Maybe UserProfile))
getProfile uidEvMb = do
  nestedDyn <- prerender (pure $ constDyn Nothing) $ do
    profileEv <- fmap decodeXhrResponse <$> sendRequest "sprofile" uidEvMb
    holdDyn Nothing profileEv
  pure $ flattenDyn nestedDyn

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

  postBuildEv <- getPostBuild
  let initUidEv    = fmapMaybe id $ tagPromptlyDyn (profileUidDyn appState) postBuildEv
      updateUidEv  = fmapMaybe id $ updated (profileUidDyn appState)
      uidEv        = leftmost [ initUidEv, updateUidEv ]
      profileReqEv = ProfileInfo <$> uidEv
  profileDyn  <- getProfile profileReqEv

  dyn_ $ ffor profileDyn $ \profMb -> do 
    case profMb of
      Nothing -> 
        elClass_ DIV "profile-container" $ do
          -- TODO: Add main page button
          loginControlButton LoginAndMain appState
          el_ H1 $ text "PROFILE PAGE"
          elClass_ DIV "not-found" $
            text "User not found"

      Just prof -> do
        elClass_ DIV "profile-container" $ do
          -- TODO: Add main page button
          elClass_ DIV "profile-buttons" $ do 
            loginControlButton LoginAndMain appState
            showEditButton appState
          el_ H1 $ text "PROFILE PAGE"
          elClass_ DIV "profile-field" $ do
            elClass_ DIV "profile-label" $ text "Username: "
            el_ DIV $ text (fromMaybe "" $ prName prof)

          -- TODO: showGregorian to dd/mm/yyyy
          elClass_ DIV "profile-field" $ do
            elClass_ DIV "profile-label" $ text "DOB (dd/mm/yyyy): "
            el_ DIV $ text (fromMaybe "" $ pack . showGregorian <$> prDOB prof)

          elClass_ DIV "profile-field" $ do
            elClass_ DIV "profile-label" $ text "City/County: "
            el_ DIV $ text (fromMaybe "" $ prLocation prof)

          elClass_ DIV "profile-field" $ do
            elClass_ DIV "profile-label" $ text "Hobbies: "
            el_ DIV $ text (fromMaybe "" $ prHobbies prof)

          elClass_ DIV "profile-field" $ do
            elClass_ DIV "profile-label" $ text "Bio: "
            el_ DIV $ text (fromMaybe "" $ prBio prof)
