{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Main where

import           Common.Api
import           Common.Route
import           Control.Monad               (void)
import           Control.Monad.Fix           (MonadFix)
import           Data.Aeson                  (eitherDecodeStrict')
import qualified Data.ByteString.Char8       as B8
import           Data.Maybe                  (fromMaybe, isJust)
import           Data.Text                   (Text, null, pack, unpack)
import           General.Buttons
import           General.Elements
import           General.Functions
import           Language.Javascript.JSaddle (liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Prelude                     hiding (div, null, span)
import           Reflex.Dom.Core             hiding (el, elAttr, elAttr')

selectCookies :: MonadWidget t m
              => Event t ()
              -> m (Event t CookieData)
selectCookies clickEv = do
  authEvent <- performEvent $ ffor clickEv $ \_ -> do
    cookieText <- liftJSM getCookies
    pure (parseCookie cookieText)
  pure authEvent

input :: (DomBuilder t m, PostBuild t m)
      => AppState t
      -> Event t Text
      -> m (InputElement EventResult (DomBuilderSpace m) t)
input appState clearEv = do
  let loggedInDyn = isJust <$> appLoggedIn appState
      attrs = ffor loggedInDyn $ \loggedIn ->
                if loggedIn
                then "disabled" =: Nothing
                else "disabled" =: Just (pack "true")
  ie <- textBox NotPassword clearEv (Hideable $ updated attrs)
  dyn_ $ ffor loggedInDyn $ \loggedIn ->
    if loggedIn
    then void $ button "📨"
    else blank
  pure ie

postAndGetMsgs :: (Applicative m, Prerender t m)
               => InputElement er d t
               -> Event t ()
               -> m (Dynamic t (Event t Text))
postAndGetMsgs inputEl loginEv =
  prerender (pure never) $ mdo
    rec
      let nameAuthEv = fromMaybe (Auth "", User "", UID 0) <$> nameAuthEvMaybe
          msgEv      = tagPromptlyDyn (_inputElement_value inputEl) loginEv
          reqEv      = attachPromptlyDynWith
                        (\msg (Auth auth, User user, UID _) -> MessageReq user 0 msg auth)
                        msgDyn
                        nameAuthEv
          initTextEv = fmap (fromMaybe "" . _xhrResponse_responseText) initEv
          triggerGet = void postEv
          getTextEv  = fmap (fromMaybe "" . _xhrResponse_responseText) getEv

-- TODO: This can probably be changed to check AppState
      nameAuthEvMaybe <- selectCookies loginEv
      msgDyn          <- holdDyn "" msgEv
      postbuild       <- getPostBuild
      initEv          <- sendRequest get postbuild
      postEv          <- sendRequest post reqEv
      getEv           <- sendRequest get triggerGet
    pure $ leftmost [initTextEv, getTextEv]

-- TODO: Make this [(User, Message)]
decodeJson :: Text -> [MessageResp]
decodeJson t =
  case eitherDecodeStrict' (B8.pack $ unpack t) of
    Left  _err              -> [] -- TODO: handle this error better
    Right (MessageResps xs) -> xs

replaceText :: Text -> Text -> [MessageResp] -> [MessageResp]
replaceText newName userName respList = ifName <$> respList
  where 
    ifName msgResp = if resUserName msgResp == userName 
                     then msgResp { resUserName = newName }
                     else msgResp

replaceUserName :: Reflex t 
                => Text 
                -> Dynamic t Text 
                -> Dynamic t [MessageResp] 
                -> Dynamic t [MessageResp]
replaceUserName newName userNameDyn respListDyn 
  = zipDynWith replace userNameDyn respListDyn
    where 
      replace userName respList = replaceText newName userName respList

displayMessages :: ( DomBuilder t m
                   , PostBuild t m
                   , MonadHold t m
                   , MonadFix m
                   , Prerender t m
                   ) => AppState t -> Dynamic t [MessageResp] -> m ()
displayMessages appState respListDyn = mdo
  -- TODO: only display buttons when logged in
  elAttr_ DIV (Class "allMessages") $ do
    rec
      let userNameDyn = username . (\(_,u,_) -> u) <$> fromMaybe (Auth "", User "", UID 0) <$> appLoggedIn appState
      let userIdDyn = userid . (\(_,_,i) -> i) <$> fromMaybe (Auth "", User "", UID 0) <$> appLoggedIn appState
          userYouListDyn = replaceUserName "You" userNameDyn respListDyn
      respDynList <- simpleList userYouListDyn $ \pairDyn -> do
        elAttr_ DIV (Class "message") $ do
          void $ dyn $ ffor pairDyn $ \msgResp -> do
            let user = resUserName msgResp
                msg  = message msgResp
            el_ SPAN $ text user
            text (": " <> msg)
            text (pack . show $ likes msgResp) -- This needs to by a dynamic
            
          likeClickEv <- button "👍"
          let zippedDyns = zipDyn userIdDyn pairDyn
              likedMsgEv 
                = attachPromptlyDynWith 
                    (\(rid, msgResp) _ -> (MessageReply Nothing (Just Like) (msgId msgResp) rid)) zippedDyns likeClickEv

          void $ prerender (pure ()) $ void $ sendRequest "supdate" likedMsgEv
          let likedThisEv 
                = attachPromptlyDynWith 
                    (\msgResp _ -> (resUserName msgResp, message msgResp, "like")) pairDyn likeClickEv

          replyClickEv <- button "↩"
          let replyEv 
                = attachPromptlyDynWith 
                    (\msgResp _ -> (resUserName msgResp, message msgResp, "replied")) pairDyn replyClickEv

          trackClickEv <- button "📌"
          let trackEv 
                = attachPromptlyDynWith 
                    (\msgResp _ -> (resUserName msgResp, message msgResp, "are tracking")) pairDyn trackClickEv

          pure $ leftmost [likedThisEv, replyEv, trackEv]

      let allLikesEv = switchDyn (leftmost <$> respDynList)
      lastLikedDyn <- holdDyn Nothing (Just <$> allLikesEv)

    el_ DIV $ dyn_ $ ffor lastLikedDyn $ \case
      Nothing             -> blank
      Just (user,msg,typ) -> el_ P $ text 
                                   $ "You " 
                                   <> typ <> ": " 
                                   <> user <> ": " 
                                   <> msg
                        
mainPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m
         => AppState t -> RoutedT t () m ()
mainPage appState = mdo
  loginControlButton LoginAndSignup appState
  el_ H1 $ text "Twitter App"
  el_ P $ text "Enter text and press submit:"

  (formEl, _) <- elAttR_ FORM (OnSubmit "return false;") $ el_ DIV $ do
    rec
      let respTextEv  = switchDyn respTextDyn
          respListEv  = fmap decodeJson respTextEv
          enterEv     = domEvent Submit formEl
          nonEmptyDyn = not . null <$> _inputElement_value inputEl
          loginEv     = gate (current nonEmptyDyn) enterEv
          clearEv     = "" <$ loginEv

      inputEl     <- el_ DIV $ input appState clearEv
      respTextDyn <- postAndGetMsgs inputEl loginEv
      respListDyn <- holdDyn [] respListEv

    displayMessages appState respListDyn
  pure ()

