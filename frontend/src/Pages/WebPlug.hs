{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.WebPlug where

import           Common.Api
import           Control.Monad         (void)
import           Data.Aeson            (eitherDecodeStrict')
import           Data.ByteString       (ByteString)
import qualified Data.Map              as M
import           Data.Text             (Text, pack)
import           Reflex.Dom.Core

decodeJson :: ByteString -> MessageRespsS
decodeJson bs =
  either (const $ MessageRespsS M.empty) id
         (eitherDecodeStrict' bs)

webPlugPage
  :: forall t m.
     ( DomBuilder t m
     , Prerender  t m
     )
  => m ()
webPlugPage = void $ prerender (pure ()) $ mdo
  el "h2" $ text "Obelisk + WebSocket Example"
  let subscribeText :: [Text]
      subscribeText = ["subscribe"]

  rec
    let cfg :: WebSocketConfig t Text
        cfg = def { _webSocketConfig_send = subscribeText <$ onOpen }
        patches = fmap decodeJson incomingText

    RawWebSocket{ _webSocket_recv = incomingText, _webSocket_open = onOpen } <-
      webSocket "ws://localhost:8000/websocket" cfg


    msgMapDyn <- foldDyn
      (\(MessageRespsS newMap) oldMap -> M.union newMap oldMap)
      M.empty
      patches

    void $ el "ul" $ listWithKey msgMapDyn $ \_ msgDyn -> el "li" $ do
      dynText (resUserNameS <$> msgDyn)
      text ": "
      dynText (messageS     <$> msgDyn)
      text " 👍 "
      dynText (pack . show . length . likesS <$> msgDyn)

  pure ()

