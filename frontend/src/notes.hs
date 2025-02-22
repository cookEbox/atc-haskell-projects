
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Twitter Clone"
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
    el "h1" $ text "Obelisk Echo App"
    el "p" $ text "Enter text and press submit:"

    input <- inputElement def
    submitBtn <- button "Send to Backend"
    let reqEvent = tag (current $ fmap (\t -> MessageReq t) $ _inputElement_value input) submitBtn


    _ <- prerender (pure ()) $ do
      postBuild <- getPostBuild
      getInitEvent <- performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> get) postBuild
      respEvent <- performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> post) reqEvent
      let getInitText = fmap (fromJustDef "" . _xhrResponse_responseText) getInitEvent
          getReqEvent = (const ()) <$> respEvent

      el "div" $ do
        getRespEvent <- performRequestAsync $ fmap (postJson $ "http://localhost:8000/" <> get) getReqEvent
        let getRespText = fmap (fromJustDef "" . _xhrResponse_responseText) getRespEvent
        dynText =<< holdDyn "" getInitText
        dynText =<< holdDyn "" getRespText
      pure ()

    pure ()
  }
