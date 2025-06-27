{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Main where

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
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Prelude                     hiding (div, null, span)
import           Reflex.Dom.Core             hiding (el, elAttr, elAttr')

decodeJsonS :: ByteString -> MessageRespsS
decodeJsonS bs =
  either (const $ MessageRespsS Replace M.empty) id
         (eitherDecodeStrict' bs)

decodeJson :: Text -> [MessageResp]
decodeJson t =
  case eitherDecodeStrict' (B8.pack $ unpack t) of
    Left  _err              -> [] 
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

buildReply :: Reflex t 
            => (MessageRespS -> Maybe Integer) 
            -> Dynamic t MessageRespS 
            -> Event t (Maybe (Auth, User, UID))
            -> ReplyType
            -> Integer 
            -> Event t MessageReply 
buildReply func msgDyn nameAuthEv replyT rid = 
  attachPromptlyDynWith 
    (\msg auu -> MessageReply Nothing replyT (func msg) rid (auth auu))
    msgDyn 
    nameAuthEv
  where 
    auth = fmap (\(Auth a, _, _    ) -> a) 

likeButton :: ( DomBuilder t m
              , MonadFix m
              , PostBuild t m
              , Prerender t m 
              ) => AppState t
                -> Dynamic t (Maybe Integer) 
                -> Dynamic t MessageRespS
                -> m ()
likeButton appState userIdDynMb mapDyn = mdo 
  dyn_ $ ffor userIdDynMb $ \case 
    Nothing -> blank
    Just rid -> do 
      rec
        (e, _) <- elR_ BUTTON $ do dyn thumbsUpDyn
        let iconSwitcher msgResp 
              = if rid `elem` likesS msgResp 
                then elClass_ I "fa-solid fa-thumbs-up" blank
                else elClass_ I "fa-regular fa-thumbs-up" blank
            thumbsUpDyn  = iconSwitcher <$> mapDyn
            likeClickEv  = domEvent Click e
            nameAuthEvMb = tagPromptlyDyn (appLoggedIn appState) likeClickEv
            likedMsgEv   = buildReply (Just . msgIdS) mapDyn nameAuthEvMb Like rid
      void $ prerender (pure ()) $ void $ sendRequest "supdate" likedMsgEv

followButton :: ( DomBuilder t m
              , MonadFix m
              , PostBuild t m
              , Prerender t m 
              ) => AppState t 
                -> Dynamic t (Maybe Integer) 
                -> Dynamic t MessageRespS
                -> m ()
followButton appState userIdDynMb mapDyn = mdo
  dyn_ $ ffor userIdDynMb $ \case 
    Nothing -> blank
    Just rid -> do 
      rec
        (e, _) <- elR_ BUTTON $ do dyn followingDyn
        let iconSwitcher msgResp 
              = if rid `elem` followsS msgResp 
                then elClass_ I "fa-solid fa-thumbtack" blank
                else elClass_ I "fa-regular fa-circle" blank
            followingDyn  = iconSwitcher <$> mapDyn
            followClickEv = domEvent Click e
            nameAuthEvMb  = tagPromptlyDyn (appLoggedIn appState) followClickEv
            followMsgEv   = buildReply resUserIdS mapDyn nameAuthEvMb Follow rid
      void $ prerender (pure ()) $ void $ sendRequest "supdate" followMsgEv

maybeFollowButton :: ( DomBuilder t m
                     , PostBuild t m
                     , MonadFix m
                     , Prerender t m 
                     ) => AppState t 
                       -> Dynamic t (Maybe Integer) 
                       -> Dynamic t MessageRespS -> m ()
maybeFollowButton appState userIdDynMb mapDyn = do
  let zippedDyn = zipDyn userIdDynMb mapDyn
  dyn_ $ ffor zippedDyn $ \(mIn, msgResp) -> do 
    let msgSenderId = resUserIdS msgResp 
    case (/=) <$> msgSenderId <*> mIn of 
      Nothing -> blank
      (Just False) -> blank 
      (Just True) -> followButton appState userIdDynMb mapDyn

printUserName :: (DomBuilder t f, PostBuild t f) 
              => Dynamic t MessageRespS -> f ()
printUserName mapDyn = do 
  void $ dyn $ ffor mapDyn $ \msgResp -> do
    let user = resUserNameS msgResp
    el_ SPAN $ text user

printLikes :: (DomBuilder t f, PostBuild t f) 
           => Dynamic t MessageRespS -> f ()
printLikes mapDyn = do 
  void $ dyn $ ffor mapDyn $ \msgResp -> do
    let printlikes = pack . show . length . likesS
    text $ printlikes msgResp 

printMessage :: (DomBuilder t f, PostBuild t f) 
             => Dynamic t MessageRespS -> f ()
printMessage mapDyn = do 
  void $ dyn $ ffor mapDyn $ \msgResp -> do
    let msg  = messageS msgResp
    text msg

classDynSw :: Reflex t 
           => Dynamic t (Maybe Integer) 
           -> Dynamic t MessageRespS 
           -> Dynamic t Text
classDynSw userIdDynMb msgDyn = zipDynWith swtch userIdDynMb msgDyn
  where 
    swtch mbUid msg =
       case (mbUid, resUserIdS msg) of
         (Just uid, Just author) | uid == author 
           -> "message message--self"
         _ -> "message message--other"

displayMessages :: ( DomBuilder t m
                   , PostBuild t m
                   , MonadHold t m
                   , MonadFix m
                   , Prerender t m
                   ) => AppState t 
                     -> Dynamic t (M.Map Integer MessageRespS) 
                     -> m (Dynamic t (Maybe Integer))
displayMessages appState respMapDyn = mdo
  uidMb <- elClass_ DIV "allMessages" $ do 
    rec 
      let loggedIn       = appLoggedIn appState 
          auuDyn         = fromMaybe (Auth "", User "", UID 0) <$> loggedIn
          userNameDyn    = username . (\(_,u,_) -> u) <$> auuDyn
          userIdDynMb    = fmap (userid . (\(_,_,i) -> i)) <$> loggedIn
          userYouListDyn = replaceUserName "You" userNameDyn respMapDyn
      void $ reverseList userYouListDyn $ \msgDyn -> do
        let classDyn = classDynSw userIdDynMb msgDyn
        elDynClass "div" classDyn $ do
          elClass_ DIV "message-header" $ do
            printUserName msgDyn
            maybeFollowButton appState userIdDynMb msgDyn
          elClass_ DIV "message-body" $ do
            printMessage msgDyn
          elClass_ DIV "message-footer" $ do 
            printLikes msgDyn
            likeButton appState userIdDynMb msgDyn
    pure userIdDynMb
  pure uidMb

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
    then elClass_ BUTTON "send-button" $
            elClass_ I "fas fa-paper-plane" blank
    else blank
  pure ie

requestEvent :: Reflex t 
             => Dynamic t Msg 
             -> Event t (Auth, User, UID) 
             -> Event t MessageReq
requestEvent msgDyn nameAuthEv = 
  attachPromptlyDynWith
    (\msg (Auth auth, User user, UID uid) -> MessageReq user uid msg auth)
    msgDyn
    nameAuthEv

postMsgs :: (Applicative m, Prerender t m)
         => AppState t
         -> InputElement er d t
         -> Event t ()
         -> m ()
postMsgs appState inputEl enterEv =
  void $ prerender (pure ()) $ mdo
    rec
      let nameAuthEvMb = tagPromptlyDyn (appLoggedIn appState) enterEv
          nameAuthEv   = fromMaybe (Auth "", User "", UID 0) <$> nameAuthEvMb
          msgEv        = tagPromptlyDyn (_inputElement_value inputEl) enterEv
          reqEv        = requestEvent msgDyn nameAuthEv
      msgDyn <- holdDyn "" msgEv
    void $ sendRequest "post" reqEv

sendTweet :: (DomBuilder t m , PostBuild t m , MonadFix m, Prerender t m) 
          => AppState t -> m ()
sendTweet appState = mdo
  (formEl, _) <- elAttR_ FORM (single $ OnSubmit "return false;") $ el_ DIV $ do
    rec
      let enterEv     = domEvent Submit formEl
          nonEmptyDyn = not . null <$> _inputElement_value inputEl
          loginEv     = gate (current nonEmptyDyn) enterEv
          clearEv     = "" <$ loginEv

      -- TODO: Needs to be an textArea
      inputEl     <- el_ DIV $ input appState clearEv
      void $ postMsgs appState inputEl loginEv
    pure ()
  pure ()

feedButtons :: DomBuilder t m 
            => Integer -> m (Event t [ClientMsg])
feedButtons uid = do
  mainClickEv    <- button "Main"
  userClickEv    <- button "My Page"
  followsClickEv <- button "Friends"
  let mainEv        = [All]       <$ mainClickEv
      userEv        = [UserMsgs uid]  <$ userClickEv
      followsEv     = [Following uid] <$ followsClickEv
  pure $ leftmost [mainEv, userEv, followsEv]

holdWidget :: (DomBuilder t m, MonadHold t m) 
           => Event t b 
           -> Dynamic t (Maybe Integer) 
           -> m (Dynamic t (Event t [ClientMsg]))
holdWidget onOpen uidMb = widgetHold
            (pure $ [All] <$ onOpen)
            (ffor (updated uidMb) $ \case
               Nothing  -> pure $ [All] <$ onOpen
               Just uid -> feedButtons uid
            )

patchOrClear :: MessageMapMb
             -> M.Map Integer MessageRespS -> M.Map Integer MessageRespS
patchOrClear Nothing                               _      = M.empty
patchOrClear (Just (MessageRespsS Replace newMap)) _      = newMap
patchOrClear (Just (MessageRespsS Patch newMap))   oldMap = M.union newMap oldMap

mainPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m
         => AppState t -> RoutedT t () m ()
mainPage appState = do
  elClass_ DIV "top-banner" $
    elClass_ DIV "banner-inner" $ do
      elClass_ DIV "banner-title" $ text "Twitter App"
      elClass_ DIV "banner-button" $ loginControlButton LoginAndSignup appState
  elClass_ DIV "main-content" $ void $ prerender (pure ()) $ mdo
    rec
      let userSendEv = switchDyn dynUserSend 
          cfg        = def { _webSocketConfig_send = userSendEv }
          clearEv    = Nothing <$ switchDyn dynUserSend 
          patchEv    = incomingText

      RawWebSocket{ _webSocket_recv = incomingText, _webSocket_open = onOpen } 
        <- jsonWebSocket "ws://localhost:8000/websocket" cfg

      msgMapDyn <- foldDyn patchOrClear M.empty (leftmost [clearEv, patchEv])

      dynUserSend <- elClass_ DIV "feed-buttons" $ holdWidget onOpen uidMb
      elClass_ DIV "tweet-box" $ sendTweet appState
      uidMb <- displayMessages appState msgMapDyn
    pure ()

