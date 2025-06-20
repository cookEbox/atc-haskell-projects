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
      initEv          <- sendRequest "get" postbuild
      postEv          <- sendRequest "post" reqEv
      getEv           <- sendRequest "get" triggerGet
    pure $ leftmost [initTextEv, getTextEv]

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

followButton :: ( DomBuilder t m
              , MonadFix m
              , PostBuild t m
              , Prerender t m 
              ) => Dynamic t (Maybe Integer) 
                -> Dynamic t MessageResp 
                -> m ()
followButton userIdDynMb pairDyn = mdo 
  dyn_ $ ffor userIdDynMb $ \case 
    Nothing -> blank
    Just rid -> do 
      rec
        (e, _) <- el' "button" $ dynText thumbsUpDyn
        let bldMsgReply msgResp 
              = MessageReply Nothing Nothing (Just Follow) (resUserId msgResp) rid 
            iconSwitcher msgResp = if rid `elem` follows msgResp 
                                   then "📌"
                                   else "📍"
            thumbsUpDyn   = iconSwitcher <$> pairDyn
            followClickEv = domEvent Click e
            msgReply      = bldMsgReply <$> pairDyn
            followMsgEv   = tagPromptlyDyn msgReply followClickEv
      void $ prerender (pure ()) $ void $ sendRequest "supdate" followMsgEv

likeButton :: ( DomBuilder t m
              , MonadFix m
              , PostBuild t m
              , Prerender t m 
              ) => Dynamic t (Maybe Integer) 
                -> Dynamic t MessageResp 
                -> m ()
likeButton userIdDynMb pairDyn = mdo 
  dyn_ $ ffor userIdDynMb $ \case 
    Nothing -> blank
    Just rid -> do 
      rec
        (e, _) <- el' "button" $ dynText thumbsUpDyn
        let bldMsgReply msgResp 
              = MessageReply Nothing (Just Like) Nothing (Just $ msgId msgResp) rid 
            iconSwitcher msgResp = if rid `elem` likes msgResp 
                                   then "👍" 
                                   else "▫️"
            thumbsUpDyn  = iconSwitcher <$> pairDyn
            likeClickEv  = domEvent Click e
            msgReply     = bldMsgReply <$> pairDyn
            likedMsgEv   = tagPromptlyDyn msgReply likeClickEv
      void $ prerender (pure ()) $ void $ sendRequest "supdate" likedMsgEv

displayMessages :: ( DomBuilder t m
                   , PostBuild t m
                   , MonadHold t m
                   , MonadFix m
                   , Prerender t m
                   ) => AppState t -> Dynamic t [MessageResp] -> m ()
displayMessages appState respListDyn = mdo
  -- TODO: Add reply button functionality
  elAttr_ DIV (Class "allMessages") $ do
    rec
      let loggedIn       = appLoggedIn appState
          auuDyn         = fromMaybe (Auth "", User "", UID 0) <$> loggedIn
          userNameDyn    = username . (\(_,u,_) -> u) <$> auuDyn
          userIdDynMb    = fmap (userid . (\(_,_,i) -> i)) <$> loggedIn
          userYouListDyn = replaceUserName "You" userNameDyn respListDyn
      void $ simpleList userYouListDyn $ \pairDyn -> do
        elAttr_ DIV (Class "message") $ do

          let zippedDyn = zipDyn userIdDynMb pairDyn
          dyn_ $ ffor zippedDyn $ \(mIn, msgResp) -> do 
            let msgSenderId = resUserId msgResp 
            case (/=) <$> msgSenderId <*> mIn of 
              Nothing -> blank
              (Just False) -> blank 
              (Just True) -> followButton userIdDynMb pairDyn

          void $ dyn $ ffor pairDyn $ \msgResp -> do
            let user = resUserName msgResp
                msg  = message msgResp
                printlikes = pack . show . length . likes 
            el_ SPAN $ text user
            text (": " <> msg)
            text (printlikes msgResp) 

          likeButton userIdDynMb pairDyn
    pure ()

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

