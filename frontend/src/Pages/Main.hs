{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Main where

import           Common.Api
import           Common.Route
import           Control.Monad               (void)
import           Data.Maybe                  (fromMaybe, isJust)
import           Data.Text                   as T
import           General.Buttons
import           General.Functions
import           Language.Javascript.JSaddle (liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core

selectCookies :: MonadWidget t m => Event t () -> m (Event t (Maybe (Text, Text)))
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
          msgEv = tagPromptlyDyn (_inputElement_value inputEl) loginEv
          reqEv = attachPromptlyDynWith
                    (\msg (auth, user) -> MessageReq user msg auth)
                    msgDyn
                    nameAuthEv
          initText = fmap (fromMaybe "" . _xhrResponse_responseText) initEv
          triggerGet = void postEv
          getText = fmap (fromMaybe "" . _xhrResponse_responseText) getEv

      nameAuthEvMaybe <- selectCookies loginEv
      msgDyn <- holdDyn "" msgEv
      postbuild <- getPostBuild
      initEv <- sendRequest get postbuild
      postEv <- sendRequest post reqEv 
      getEv <- sendRequest get triggerGet
    pure $ leftmost [initText, getText]

mainPage :: forall t (m :: * -> *). ObeliskWidget t (R FrontendRoute) m 
         => AppState t -> RoutedT t () m ()
mainPage appState = mdo
  loginControlButton InAndOut appState
  el "h1" $ text "Twitter App"
  el "p" $ text "Enter text and press submit:"

  (formEl, _) <- elAttr' "form" ("onsubmit" =: "return false;") $ el "div" $ do
    rec
      let respTextEv = switchDyn respTextDyn
          enterEv = domEvent Submit formEl
          nonEmpty = not . T.null <$> _inputElement_value inputEl
          loginEv = gate (current nonEmpty) enterEv
          clearEv = "" <$ loginEv

      inputEl <- el "div" $ input appState clearEv
      respTextDyn <- postAndGetMsgs inputEl loginEv 
      displayDyn <- holdDyn "Loading...." respTextEv
      el "div" $ dynText displayDyn
    pure ()
  pure ()

