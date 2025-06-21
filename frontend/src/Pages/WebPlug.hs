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
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text, pack)
import           General.Buttons
import           General.Elements
import           General.Functions
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Prelude                     hiding (div, null, span)
import           Reflex.Dom.Core             hiding (el, elAttr, elAttr')

decodeJson :: ByteString -> MessageRespsS
decodeJson bs =
  either (const $ MessageRespsS M.empty) id
         (eitherDecodeStrict' bs)

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

replaceText :: Text -> Text -> (M.Map Integer MessageRespS) -> (M.Map Integer MessageRespS)
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

-- TODO: Fix this so it updates all message 
-- TODO: Make this only visible on other accounts not you
-- followButton :: ( DomBuilder t m
--               , MonadFix m
--               , PostBuild t m
--               , Prerender t m 
--               ) => Dynamic t (Maybe Integer) 
--                 -> Dynamic t MessageRespS
--                 -> m ()
-- followButton userIdDynMb mapDyn = mdo 
--   dyn_ $ ffor userIdDynMb $ \case 
--     Nothing -> blank
--     Just rid -> do 
--       rec
--         (e, _) <- el' "button" $ dynText thumbsUpDyn
--         let bldMsgReply msgResp 
--               = MessageReply Nothing Nothing (Just Follow) (resUserIdS msgResp) rid 
--             iconSwitcher msgResp = if rid `elem` followsS msgResp 
--                                    then "📌"
--                                    else "📍"
--             thumbsUpDyn   = iconSwitcher <$> mapDyn
--             followClickEv = domEvent Click e
--             msgReply      = bldMsgReply <$> mapDyn
--             followMsgEv   = tagPromptlyDyn msgReply followClickEv
--       void $ prerender (pure ()) $ void $ sendRequest "supdate" followMsgEv

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
        let bldMsgReply msgResp 
              = MessageReply Nothing (Just Like) Nothing (Just $ msgIdS msgResp) rid 
            iconSwitcher msgResp = if rid `elem` likesS msgResp 
                                   then "👍" 
                                   else "▫️"
            thumbsUpDyn  = iconSwitcher <$> mapDyn
            likeClickEv  = domEvent Click e
            msgReply     = bldMsgReply <$> mapDyn
            likedMsgEv   = tagPromptlyDyn msgReply likeClickEv
      void $ prerender (pure ()) $ void $ sendRequest "supdate" likedMsgEv

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
          -- followButton userIdDynMb mapDyn
        
          void $ dyn $ ffor mapDyn $ \msgResp -> do
            let user = resUserNameS msgResp
                msg  = messageS msgResp
                printlikes = pack . show . length . likesS
            el_ SPAN $ text user
            text (": " <> msg)
            text (printlikes msgResp) 
          pure () 
          likeButton userIdDynMb mapDyn
    pure ()
  pure ()

webPlugPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m
         => AppState t -> RoutedT t () m ()
webPlugPage appState = do
  el_ H2 $ text "Obelisk + WebSocket Example"
  loginControlButton LoginAndSignup appState
  void $ prerender (pure ()) $ mdo
    rec
      let subscribeText = ["subscribe"] :: [Text]
          cfg = def { _webSocketConfig_send = subscribeText <$ onOpen }
          patches = fmap decodeJson incomingText
      RawWebSocket{ _webSocket_recv = incomingText, _webSocket_open = onOpen } 
        <- webSocket "ws://localhost:8000/websocket" cfg
      msgMapDyn <- foldDyn
        (\(MessageRespsS newMap) oldMap -> M.union newMap oldMap)
        M.empty
        patches
      displayMessages appState msgMapDyn
    pure ()

