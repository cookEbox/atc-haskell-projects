{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.ProfileShared.GetProfile where

import           Common.Api
import           General.Functions
import           Reflex.Dom.Core

getProfile :: (Monad m, Prerender t m)
           => Event t ProfileInfo
           -> m (Dynamic t (Maybe UserProfile))
getProfile uidEvMb = do
  nestedDyn <- prerender (pure $ constDyn Nothing) $ do
    profileEv <- fmap decodeXhrResponse <$> sendRequest "sprofile" uidEvMb
    holdDyn Nothing profileEv
  pure $ flattenDyn nestedDyn

getProfileDyn :: (PostBuild t m, Prerender t m) 
              => AppState t -> m (Dynamic t (Maybe UserProfile))
getProfileDyn appState = do 
  postBuildEv <- getPostBuild
  let initUidEv    = fmapMaybe id $ tagPromptlyDyn (profileUidDyn appState) postBuildEv
      updateUidEv  = fmapMaybe id $ updated (profileUidDyn appState)
      uidEv        = leftmost [ initUidEv, updateUidEv ]
      profileReqEv = ProfileInfo <$> uidEv
  getProfile profileReqEv

