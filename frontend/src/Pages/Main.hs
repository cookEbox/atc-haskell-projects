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
import           Control.Monad                 (void)
import           Data.Aeson                    (eitherDecodeStrict')
import qualified Data.ByteString.Char8         as B8
import           Data.Maybe                    (fromMaybe, isJust)
import           Data.Text                     (Text, null, pack, unpack)
import           General.Buttons
import           General.Functions
import           Language.Javascript.JSaddle   (liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Prelude                       hiding (div, null, span)
import           Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)

selectCookies :: MonadWidget t m
              => Event t ()
              -> m (Event t (Maybe (Text, Text)))
selectCookies click = do
  authEvent <- performEvent $ ffor click $ \_ -> do
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
      let nameAuthEv = fromMaybe ("", "") <$> nameAuthEvMaybe
          msgEv      = tagPromptlyDyn (_inputElement_value inputEl) loginEv
          reqEv      = attachPromptlyDynWith
                        (\msg (auth, user) -> MessageReq user msg auth)
                        msgDyn
                        nameAuthEv
          initText   = fmap (fromMaybe "" . _xhrResponse_responseText) initEv
          triggerGet = void postEv
          getText    = fmap (fromMaybe "" . _xhrResponse_responseText) getEv

      nameAuthEvMaybe <- selectCookies loginEv
      msgDyn          <- holdDyn "" msgEv
      postbuild       <- getPostBuild
      initEv          <- sendRequest get postbuild
      postEv          <- sendRequest post reqEv
      getEv           <- sendRequest get triggerGet
    pure $ leftmost [initText, getText]

decodeJson :: Text -> [(Text,Text)]
decodeJson t =
  case eitherDecodeStrict' (B8.pack $ unpack t) of
    Left  _err             -> []
    Right (MessageResp xs) -> xs

displayMessages :: ( DomBuilder t m
                   , PostBuild t m
                   , MonadHold t m
                   , MonadFix m 
                   ) => Dynamic t [(Text, Text)] -> m ()
displayMessages respListDyn = 
  elAttr "div" ("class" =: "allMessages") $ do
    likeDynList <- simpleList respListDyn $ \pairDyn -> do
      elAttr "div" ("class" =: "message") $ do
        void $ dyn $ ffor pairDyn $ \(user, msg) -> do
          el "span" $ text user
          text (": " <> msg)

        likeClickEv <- button "👍"
        let likedThisEv = attachPromptlyDynWith (\(user, msg) _ -> (user, msg)) pairDyn likeClickEv

        pure likedThisEv

    let allLikesEv = switchPromptlyDyn (leftmost <$> likeDynList)
    lastLikedDyn <- holdDyn Nothing (Just <$> allLikesEv)

    el "div" $ dyn_ $ ffor lastLikedDyn $ \case
      Nothing       -> blank
      Just (user,m) -> el "p" $ text $ "You liked: " <> user <> ": " <> m

mainPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m
         => AppState t -> RoutedT t () m ()
mainPage appState = mdo
  loginControlButton LoginAndSignup appState
  el "h1" $ text "Twitter App"
  el "p" $ text "Enter text and press submit:"

  (formEl, _) <- elAttr' "form" ("onsubmit" =: "return false;") $ el "div" $ do
    rec
      let respTextEv = switchDyn respTextDyn
          respListEv = fmap decodeJson respTextEv
          enterEv    = domEvent Submit formEl
          nonEmpty   = not . null <$> _inputElement_value inputEl
          loginEv    = gate (current nonEmpty) enterEv
          clearEv    = "" <$ loginEv

      inputEl     <- el "div" $ input appState clearEv
      respTextDyn <- postAndGetMsgs inputEl loginEv
      respListDyn <- holdDyn [] respListEv

    displayMessages respListDyn
  pure ()

