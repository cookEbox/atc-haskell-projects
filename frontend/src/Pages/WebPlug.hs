{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.WebPlug where

import           Common.Api
import           Common.Route
import           Control.Monad               (void)
import           Control.Monad.Fix           (MonadFix)
import           Data.Aeson                  (eitherDecodeStrict')
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as B8
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe, isJust)
import           Data.Text                   (Text, pack, null, unpack)
import           General.Buttons
import           General.Elements
import           General.Functions
import           Language.Javascript.JSaddle (liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Prelude                     hiding (div, null, span)
import           Reflex.Dom.Core             hiding (el, elAttr, elAttr')

decodeJsonS :: ByteString -> MessageRespsS
decodeJsonS bs =
  either (const $ MessageRespsS M.empty) id
         (eitherDecodeStrict' bs)

decodeJson :: Text -> [MessageResp]
decodeJson t =
  case eitherDecodeStrict' (B8.pack $ unpack t) of
    Left  _err              -> [] -- TODO: handle this error better
    Right (MessageResps xs) -> xs

reverseList :: forall t m k v a. 
             ( Adjustable t m
             , PostBuild t m
             , MonadHold t m
             , MonadFix m
             , Eq v 
             ) => Dynamic t (M.Map k v) 
               -> (Dynamic t v -> m a) 
               -> m (Dynamic t [a])
reverseList = simpleList . (fmap . fmap $ snd) . fmap M.toDescList

replaceText :: Text 
            -> Text 
            -> M.Map Integer MessageRespS 
            -> M.Map Integer MessageRespS
replaceText newName userName respMap = ifName <$> respMap
  where 
    ifName msgResp = if resUserNameS msgResp == userName 
                     then msgResp { resUserNameS = newName }
                     else msgResp

replaceUserName :: Reflex t 
                => Text 
                -> Dynamic t Text 
                -> Dynamic t (M.Map Integer MessageRespS)
                -> Dynamic t (M.Map Integer MessageRespS)
replaceUserName newName userNameDyn respMapDyn 
  = zipDynWith replace userNameDyn respMapDyn
    where 
      replace userName respList = replaceText newName userName respList

buildReply :: (MessageRespS -> Maybe Integer) 
           -> MessageRespS 
           -> Integer 
           -> MessageReply
buildReply func msgResp rid = 
  MessageReply Nothing (Just Like) Nothing (func msgResp) rid 

likeButton :: ( DomBuilder t m
              , MonadFix m
              , PostBuild t m
              , Prerender t m 
              ) => Dynamic t (Maybe Integer) 
                -> Dynamic t MessageRespS
                -> m ()
likeButton userIdDynMb mapDyn = mdo 
  dyn_ $ ffor userIdDynMb $ \case 
    Nothing -> blank
    Just rid -> do 
      rec
        (e, _) <- el' "button" $ dynText thumbsUpDyn
        let bldMsgReply msgResp = buildReply (Just . msgIdS) msgResp rid 
            iconSwitcher msgResp = if rid `elem` likesS msgResp 
                                   then "👍" 
                                   else "▫️"
            thumbsUpDyn  = iconSwitcher <$> mapDyn
            likeClickEv  = domEvent Click e
            msgReply     = bldMsgReply <$> mapDyn
            likedMsgEv   = tagPromptlyDyn msgReply likeClickEv
      void $ prerender (pure ()) $ void $ sendRequest "supdate" likedMsgEv

followButton :: ( DomBuilder t m
              , MonadFix m
              , PostBuild t m
              , Prerender t m 
              ) => Dynamic t (Maybe Integer) 
                -> Dynamic t MessageRespS
                -> m ()
followButton userIdDynMb mapDyn = mdo 
  dyn_ $ ffor userIdDynMb $ \case 
    Nothing -> blank
    Just rid -> do 
      rec
        (e, _) <- el' "button" $ dynText thumbsUpDyn
        let bldMsgReply msgResp = buildReply resUserIdS msgResp rid 
            iconSwitcher msgResp = if rid `elem` followsS msgResp 
                                   then "📌"
                                   else "📍"
            thumbsUpDyn   = iconSwitcher <$> mapDyn
            followClickEv = domEvent Click e
            msgReply      = bldMsgReply <$> mapDyn
            followMsgEv   = tagPromptlyDyn msgReply followClickEv
      void $ prerender (pure ()) $ void $ sendRequest "supdate" followMsgEv

maybeFollowButton :: (DomBuilder t m, PostBuild t m, MonadFix m, Prerender t m) 
                  => Dynamic t (Maybe Integer) -> Dynamic t MessageRespS -> m ()
maybeFollowButton userIdDynMb mapDyn = do
  let zippedDyn = zipDyn userIdDynMb mapDyn
  dyn_ $ ffor zippedDyn $ \(mIn, msgResp) -> do 
    let msgSenderId = resUserIdS msgResp 
    case (/=) <$> msgSenderId <*> mIn of 
      Nothing -> blank
      (Just False) -> blank 
      (Just True) -> followButton userIdDynMb mapDyn

printMessage :: (DomBuilder t f, PostBuild t f) 
             => Dynamic t MessageRespS -> f ()
printMessage mapDyn = do 
  void $ dyn $ ffor mapDyn $ \msgResp -> do
    let user = resUserNameS msgResp
        msg  = messageS msgResp
        printlikes = pack . show . length . likesS
    el_ SPAN $ text user
    text (": " <> msg)
    text (printlikes msgResp) 

displayMessages :: ( DomBuilder t m
                   , PostBuild t m
                   , MonadHold t m
                   , MonadFix m
                   , Prerender t m
                   ) => AppState t 
                     -> Dynamic t (M.Map Integer MessageRespS) 
                     -> m ()
displayMessages appState respMapDyn = mdo
  elAttr_ DIV (Class "allMessages") $ do 
    rec 
      let loggedIn       = appLoggedIn appState 
          auuDyn         = fromMaybe (Auth "", User "", UID 0) <$> loggedIn
          userNameDyn    = username . (\(_,u,_) -> u) <$> auuDyn
          userIdDynMb    = fmap (userid . (\(_,_,i) -> i)) <$> loggedIn
          userYouListDyn = replaceUserName "You" userNameDyn respMapDyn
      void $ reverseList userYouListDyn $ \mapDyn -> do
        elAttr_ DIV (Class "message") $ do
          maybeFollowButton userIdDynMb mapDyn
          printMessage mapDyn
          likeButton userIdDynMb mapDyn
    pure ()
  pure ()

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

requestEvent :: Reflex t 
             => Dynamic t Msg 
             -> Event t (Auth, User, UID) 
             -> Event t MessageReq
requestEvent msgDyn nameAuthEv = 
  attachPromptlyDynWith
    (\msg (Auth auth, User user, UID _) -> MessageReq user 0 msg auth)
    msgDyn
    nameAuthEv

postAndGetMsgs :: (Applicative m, Prerender t m)
               => InputElement er d t
               -> Event t ()
               -> m (Dynamic t (Event t Text))
postAndGetMsgs inputEl loginEv =
  prerender (pure never) $ mdo
    rec
      let nameAuthEv = fromMaybe (Auth "", User "", UID 0) <$> nameAuthEvMaybe
          msgEv      = tagPromptlyDyn (_inputElement_value inputEl) loginEv
          reqEv      = requestEvent msgDyn nameAuthEv
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

sendTweet :: (DomBuilder t m , PostBuild t m , MonadFix m, Prerender t m) 
          => AppState t -> m ()
sendTweet appState = mdo
  (formEl, _) <- elAttR_ FORM (OnSubmit "return false;") $ el_ DIV $ do
    rec
      let enterEv     = domEvent Submit formEl
          nonEmptyDyn = not . null <$> _inputElement_value inputEl
          loginEv     = gate (current nonEmptyDyn) enterEv
          clearEv     = "" <$ loginEv

      inputEl     <- el_ DIV $ input appState clearEv
      void $ postAndGetMsgs inputEl loginEv
    pure ()
  pure ()

webPlugPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m
            => AppState t -> RoutedT t () m ()
webPlugPage appState = do
  loginControlButton LoginAndSignup appState
  el_ H1 $ text "Twitter App"
  el_ P $ text "Enter text and press submit:"
  sendTweet appState
  void $ prerender (pure ()) $ mdo
    rec
      let subscribeText = ["subscribe"] :: [Text]
          cfg = def { _webSocketConfig_send = subscribeText <$ onOpen }
          patches = fmap decodeJsonS incomingText
      RawWebSocket{ _webSocket_recv = incomingText, _webSocket_open = onOpen } 
        <- webSocket "ws://localhost:8000/websocket" cfg
      msgMapDyn <- foldDyn
        (\(MessageRespsS newMap) oldMap -> M.union newMap oldMap)
        M.empty
        patches
      displayMessages appState msgMapDyn
    pure ()

