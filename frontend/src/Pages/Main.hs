{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Main (mainPage) where

import           Common.Api
import           Common.Route
import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Fix           (MonadFix)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe, isJust)
import           Data.Text                   (Text, pack, null, unlines)
import           General.Buttons
import           General.Elements
import           General.Functions
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Prelude                     hiding (div, null, span, unlines)
import           Reflex.Dom.Core             hiding (el, elAttr, elAttr')

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
        (e, _) <- elR_ BUTTON $ do dyn thumbsUpDyn
        let bldMsgReply msgResp = buildReply (Just . msgIdS) Like msgResp rid
            iconSwitcher msgResp 
              = if rid `elem` likesS msgResp 
                then elClass_ I "fa-solid fa-thumbs-up" blank
                else elClass_ I "fa-regular fa-thumbs-up" blank
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
        (e, _) <- elR_ BUTTON $ do dyn followingDyn
        let bldMsgReply msgResp = buildReply resUserIdS Follow msgResp rid 
            iconSwitcher msgResp 
              = if rid `elem` followsS msgResp 
                then elClass_ I "fa-solid fa-thumbtack" blank
                else elClass_ I "fa-regular fa-circle" blank
            followingDyn  = iconSwitcher <$> mapDyn
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

printUserNameLinked :: ( PerformEvent t1 f
                       , SetRoute t1 (R FrontendRoute) f
                       , MonadIO (Performable f)
                       , DomBuilder t1 f
                       , PostBuild t1 f
                       ) => AppState t2 -> Dynamic t1 MessageRespS -> f ()
printUserNameLinked appState msgDyn = do
  let authorNameDyn = fmap (resUserNameS) msgDyn
      authorIdDyn   = fmap resUserIdS     msgDyn

  void $ dyn $ ffor (zipDynWith (,) authorNameDyn authorIdDyn) $ \(nm, mbId) ->
    case mbId of
      Just uid -> do
        (el, _) <- elAttR_ A (toAttrisList [Href "#", Class "username-link"]) (text nm)
        let clickE     = domEvent Click el
            routeE     = FrontendRoute_Profile :/ () <$ clickE
            profileE   = uid <$ clickE
        setRoute routeE
        performEvent_ $ liftIO . profileClick appState <$> profileE

      Nothing -> text "unknown user"

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
                   , PerformEvent t m
                   , MonadIO (Performable m)
                   , SetRoute t (R FrontendRoute) m
                   ) => AppState t 
                     -> Dynamic t (M.Map Integer MessageRespS) 
                     -> m (Dynamic t (Maybe Integer))
displayMessages appState respMapDyn = mdo
  uidMb <- elClass_ DIV "allMessages" $ do 
    rec 
      let loggedInDyn    = appLoggedIn appState 
          userIdDynMb    = fmap uiId <$> loggedInDyn
          uiDyn          = fromMaybe (UserInfo "" 0) <$> loggedInDyn
          userNameDyn    = uiName <$> uiDyn
          userYouListDyn = replaceUserName "You" userNameDyn respMapDyn
      void $ reverseList userYouListDyn $ \msgDyn -> do
        let classDyn = classDynSw userIdDynMb msgDyn
        elDynClass_ DIV classDyn $ do
          elClass_ DIV "message-header" $ do
            printUserNameLinked appState msgDyn
            maybeFollowButton userIdDynMb msgDyn
          elClass_ DIV "message-body" $ do
            printMessage msgDyn
          elClass_ DIV "message-footer" $ do 
            printLikes msgDyn
            likeButton userIdDynMb msgDyn
    pure userIdDynMb
  pure uidMb

textAreaMb :: MonadWidget t m 
           => AppState t 
           -> Event t Text
           -> m (TextAreaElement EventResult (DomBuilderSpace m) t)
textAreaMb appState clearEv = do 
  postBuildEv <- getPostBuild 
  let baseAttrs = M.fromList
        [ ("rows"     , Just "5")
        , ("cols"     , Just "40")
        , ("maxlength", Just "256")
        ]

      jsCtrlEnter = unlines
        [ "if(event.keyCode===13 && event.ctrlKey){"
        , "  this.form.dispatchEvent("
        , "    new Event('submit',{cancelable:true})"
        , "  );"
        , "}" 
        ]

      attrsDyn = ffor (appLoggedIn appState) $ \loggedIn ->
        let withReadonly = if isJust loggedIn
                           then M.insert "readonly" Nothing baseAttrs
                           else M.insert "readonly" (Just "") baseAttrs
        in M.insert "onkeydown" (Just jsCtrlEnter) withReadonly

      attrsEv = leftmost
        [ tagPromptlyDyn attrsDyn postBuildEv
        , updated attrsDyn
        ]

      cfg = def & textAreaElementConfig_initialValue .~ ""
                & textAreaElementConfig_setValue     .~ clearEv
                & modifyAttributes                   .~ attrsEv

  textAreaElement cfg

input :: MonadWidget t m
      => AppState t
      -> Event t Text
      -> m (TextAreaElement EventResult (DomBuilderSpace m) t)
input appState clearEv = do
  let loggedInDyn = isJust <$> appLoggedIn appState
  ie <- textAreaMb appState clearEv
  dyn_ $ ffor loggedInDyn $ \loggedIn ->
    if loggedIn
    then elClass_ BUTTON "send-button" $
            elClass_ I "fas fa-paper-plane" blank
    else blank
  pure ie

requestEvent :: Reflex t 
             => Dynamic t Msg 
             -> Event t UserInfo 
             -> Event t MessageReq
requestEvent msgDyn nameAuthEv = 
  attachPromptlyDynWith
    (\msg (UserInfo user uid) -> MessageReq user uid msg)
    msgDyn
    nameAuthEv

postMsgs :: (Applicative m, Prerender t m)
         => AppState t
         -> TextAreaElement EventResult (DomBuilderSpace m) t
         -> Event t ()
         -> m ()
postMsgs appState inputEl enterEv =
  void $ prerender (pure ()) $ mdo
  rec
    let nameAuthEvMb = tagPromptlyDyn (appLoggedIn appState) enterEv
        nameAuthEv   = fromMaybe (UserInfo "" 0) <$> nameAuthEvMb
        msgEv        = tagPromptlyDyn (_textAreaElement_value inputEl) enterEv
        reqEv        = requestEvent msgDyn nameAuthEv
    msgDyn <- holdDyn "" msgEv
  void $ sendRequest "post" reqEv

sendTweet :: (MonadWidget t m, Prerender t m) 
          => AppState t -> m ()
sendTweet appState = mdo
  (formEl, _) <- elAttR_ FORM (toAttrisList $ OnSubmit "return false;") $ el_ DIV $ do
    rec
      let enterEv     = domEvent Submit formEl
          nonEmptyDyn = not . null <$> _textAreaElement_value inputEl
          loginEv     = gate (current nonEmptyDyn) enterEv
          clearEv     = "" <$ loginEv

      inputEl <- el_ DIV $ input appState clearEv
    void $ postMsgs appState inputEl loginEv
  pure ()

mainFeedButton :: MonadWidget t m 
               => m (Element EventResult (DomBuilderSpace m) t)
mainFeedButton = do 
  (allEl, _) <- elAttR_ INPUT ( toAttrisList
    [ Type    "radio"
    , Name    "feed"
    , Id      "feed-all"
    , Value   "all"
    , Checked ""
    ]) blank
  elAttr_ LABEL (toAttrisList $ For "feed-all") $ text "All"
  return allEl

myPageFeedButton :: MonadWidget t m 
                 => m (Element EventResult (DomBuilderSpace m) t)
myPageFeedButton = do 
  (myPageEl, _) <- elAttR_ INPUT ( toAttrisList
    [ Type  "radio"
    , Name  "feed"
    , Id    "feed-mine"
    , Value "mine"
    ]) blank
  elAttr_ LABEL (toAttrisList $ For "feed-mine") $ text "Mine"
  return myPageEl

friendFeedButton :: MonadWidget t m 
                 => m (Element EventResult (DomBuilderSpace m) t)
friendFeedButton = do 
  (friendEl, _) <- elAttR_ INPUT ( toAttrisList
    [ Type  "radio"
    , Name  "feed"
    , Id    "feed-friends"
    , Value "friends"
    ]) blank
  elAttr_ LABEL (toAttrisList $ For "feed-friends") $ text "Friends"
  return friendEl

feedButtons :: MonadWidget t m 
            => Integer -> m (Event t [ClientMsg])
feedButtons uid = do
  mainClickEv    <- mainFeedButton
  userClickEv    <- myPageFeedButton
  followsClickEv <- friendFeedButton
  let mainEv        = [All]           <$ domEvent Click mainClickEv
      userEv        = [UserMsgs uid]  <$ domEvent Click userClickEv
      followsEv     = [Following uid] <$ domEvent Click followsClickEv
  pure $ leftmost [mainEv, userEv, followsEv]

holdWidget :: (MonadWidget t m) 
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
      elClass_ DIV "banner-button" $ loginControlButton LoginAndSignup LogoutAndProfile appState

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
