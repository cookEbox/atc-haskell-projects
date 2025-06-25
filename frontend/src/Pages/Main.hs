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
           -> ReplyType
           -> MessageRespS 
           -> Integer 
           -> MessageReply
buildReply func replyT msgResp rid = 
  MessageReply Nothing replyT (func msgResp) rid 

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
        let bldMsgReply msgResp = buildReply (Just . msgIdS) Like msgResp rid 
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
        let bldMsgReply msgResp = buildReply resUserIdS Follow msgResp rid 
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
                     -> m (Dynamic t (Maybe Integer))
displayMessages appState respMapDyn = mdo
  uidMb <- elAttr_ DIV (Class "allMessages") $ do 
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
    pure (userIdDynMb)
  pure (uidMb)

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
  (formEl, _) <- elAttR_ FORM (OnSubmit "return false;") $ el_ DIV $ do
    rec
      let enterEv     = domEvent Submit formEl
          nonEmptyDyn = not . null <$> _inputElement_value inputEl
          loginEv     = gate (current nonEmptyDyn) enterEv
          clearEv     = "" <$ loginEv

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

patchOrClear :: MessageRespsS
             -> M.Map Integer MessageRespS -> M.Map Integer MessageRespS
patchOrClear ClearMap               _      =      M.empty
patchOrClear (MessageRespsS newMap) oldMap = M.union newMap oldMap

mainPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m
         => AppState t -> RoutedT t () m ()
mainPage appState = do
  loginControlButton LoginAndSignup appState
  el_ H1 $ text "Twitter App"
  void $ prerender (pure ()) $ mdo
    rec
      dynUserSend <- holdWidget onOpen uidMb

      let userSendEv = switchDyn dynUserSend 
          cfg        = def { _webSocketConfig_send = userSendEv }
          clearEv    = ClearMap <$ switchDyn dynUserSend 
          patchEv  = fromMaybe (MessageRespsS M.empty) <$> incomingText

      RawWebSocket{ _webSocket_recv = incomingText, _webSocket_open = onOpen } 
        <- jsonWebSocket "ws://localhost:8000/websocket" cfg

      msgMapDyn <- foldDyn patchOrClear M.empty (leftmost [clearEv, patchEv])

      el_ P $ text "Enter text and press submit:"
      sendTweet appState
      uidMb <- displayMessages appState msgMapDyn
    pure ()

